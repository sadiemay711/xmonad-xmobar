-----------------------------------------------------------------------
--                                                                       --
--     _|      _|  _|      _|                                      _|    --
--       _|  _|    _|_|  _|_|    _|_|    _|_|_|      _|_|_|    _|_|_|    --
--         _|      _|  _|  _|  _|    _|  _|    _|  _|    _|  _|    _|    --
--       _|  _|    _|      _|  _|    _|  _|    _|  _|    _|  _|    _|    --
--     _|      _|  _|      _|    _|_|    _|    _|    _|_|_|    _|_|_|    --
--                                                                       --
---------------------------------------------------------------------------
--Huge thanks to https://github.com/randomthought/xmonad-config cause a lot of this is thier config

import System.IO
import System.Exit

import qualified Data.List as L

import SideDecorations
import XMonad
import XMonad.Actions.UpdatePointer
import XMonad.Actions.CopyWindow
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ServerMode
import XMonad.Hooks.EwmhDesktops


import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.IndependentScreens
import XMonad.Layout.ShowWName
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Cursor
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.DwmStyle
import XMonad.Layout.TabBarDecoration

import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Control.Monad (unless, when)
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import XMonad.Hooks.RefocusLast 
import XMonad.Util.NamedScratchpad
import XMonad.Layout.Decoration

import qualified Data.Map.Strict             as Map
import qualified XMonad.StackSet             as W
import qualified XMonad.Util.ExtensibleState as XS

-- Xresources/pywal integration

import Data.Maybe
import Data.Bifunctor
import Data.List as DL
import Data.Char as DC

import System.IO
import System.IO.Unsafe

import XMonad
import XMonad.Util.Run

getFromXres :: String -> IO String
getFromXres key = fromMaybe "" . findValue key <$> runProcessWithInput "xrdb" ["-query"] ""
  where
    findValue :: String -> String -> Maybe String
    findValue xresKey xres =
      snd <$> (
                DL.find ((== xresKey) . fst)
                $ catMaybes
                $ splitAtColon
                <$> lines xres
              )

    splitAtColon :: String -> Maybe (String, String)
    splitAtColon str = splitAtTrimming str <$> (DL.elemIndex ':' str)

    splitAtTrimming :: String -> Int -> (String, String)
    splitAtTrimming str idx = bimap trim trim . (second tail) $ splitAt idx str

    trim :: String -> String
    trim = DL.dropWhileEnd (DC.isSpace) . DL.dropWhile (DC.isSpace)

fromXres :: String -> String
fromXres = unsafePerformIO . getFromXres

xmobarBgColor = fromXres "*.color0"
xmobarFgColor = fromXres "*.color7"


----------------------------mupdf--------------------------------------------
-- Terminimport XMonad.Hooks.EwmhDesktopsal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "st"

-- The command to lock the screen or show the screensaver.
myScreensaver = "betterlockscreen -l"

-- The command to take a selective screenshot, where you select
-- what you'd like to capture on the screen.
mySelectScreenshot = "maimpick"

-- The command to take a fullscreen screenshot.
myScreenshot = "maim pic-full-$(date '+%y%m%d-%H%M-%S').png"

-- The command to use as a launcher, to launch commands that don't have
-- preset keybindings.
myLauncher = "dmen"

brightnessUp = "brightnessctl s +10%"

brightnessDown = "brightnessctl s 10%-"

browser = "firefox"
------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces =  withScreens 2 ["1","www","dev","chat","vid","mus"] 


-------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "pavucontrol" spawnPavu findPavu managePavu
                , NS "weechat" spawnChat findChat manageChat
                ]
  where
    spawnTerm  = myTerminal ++ " -n scratchpad"
    findTerm   = appName =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnPavu  = "pavucontrol"
    findPavu   = className =? "Pavucontrol"
    managePavu = customFloating $ W.RationalRect l t w h
               where
                 h = 0.6
                 w = 0.6
                 t = 0.65 -h
                 l = 0.65 -w

    spawnChat = myTerminal ++ " -n weechat -e weechat"
    findChat = appName =? "weechat"
    manageChat = customFloating $ W.RationalRect l t w h 
               where
                   h = 0.9
                   w = 0.9
                   t = 0.95 -h
                   l = 0.95 -w
------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [
      className =? "Navigator, Firefox"           --> doShift "www"
    , resource  =? "desktop_window"               --> doIgnore
    --, appName   =? "weechat"                      --> doShift "chat"
    , className =? "strawberry"                   --> doShift "mus"
    , className =? "Galculator"                   --> doCenterFloat
    , className =? "Steam"                        --> doCenterFloat
    , className =? "Gimp"                         --> doCenterFloat
    , resource  =? "gpicview"                     --> doCenterFloat
    , className =? "MPlayer"                      --> doCenterFloat
    , className =? "Pavucontrol"                  --> doCenterFloat
    , className =? "Mate-power-preferences"       --> doCenterFloat
    , className =? "Xfce4-power-manager-settings" --> doCenterFloat
    , className =? "VirtualBox"                   --> doShift "dev"
    , className =? "stalonetray"                  --> doIgnore
    ] <+> namedScratchpadManageHook myScratchPads

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Ubuntu:bold:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#ffffff"
    }


------------------------------------------------------------------------
-- Layouts
myLayout    = tiled ||| Full   
    where tiled = spacingRaw True (Border 0 15 10 10) True (Border 5 5 5 5) True $ avoidStruts  $ Tall nmaster delta ratio
          nmaster = 1 
          ratio = 1/2
          delta = 3/100
----------------------------------------------------------------------
-- Colors and borders

-- Color of current window title in xmobar.
xmobarTitleColor = fromXres"*.color9"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = fromXres "*.color11"

-- Width of the window border in pixels.
myBorderWidth = 0

myNormalBorderColor     = "#000000"
myFocusedBorderColor    = active

base03  = fromXres "*.foreground"
base02  = fromXres "*.background"
base01  = fromXres "*.color0"
base00  = fromXres "*.color1"
base0   = fromXres "*.color2"  
base1   = fromXres "*.color3"
base2   = fromXres "*.color4"
base3   = fromXres "*.color5"
yellow  = fromXres "*.color6"
orange  = fromXres "*.color7"
red     = fromXres "*.color8"
magenta = fromXres "*.color9"
violet  = fromXres "*.color10"
blue    = fromXres "*.color11"
cyan    = fromXres "*.color12"
green   = fromXres "*.color13"

-- sizes
gap         = 10
topbar      = 10
border      = 0
prompt      = 20
status      = 20

active      = base03 
activeWarn  = red
inactive    = magenta 
focusColor  = magenta
unfocusColor = red 

-- myFont      = "-*-Zekton-medium-*-*-*-*-160-*-*-*-*-*-*"
-- myBigFont   = "-*-Zekton-medium-*-*-*-*-240-*-*-*-*-*-*"
myFont      = "xft:Zekton:size=9:bold:antialias=true"
myBigFont   = "xft:Zekton:size=9:bold:antialias=true"
myWideFont  = "xft:Eurostar Black Extended:"
            ++ "style=Regular:pixelsize=180:hinting=true"

-- this is a "fake title" used as a highlight bar in lieu of full borders
-- (I find this a cleaner and less visually intrusive solution)
topBarTheme = def
      {
          fontName              = myFont
        , inactiveBorderColor   = inactive
        , inactiveColor         = inactive 
        , inactiveTextColor     = inactive
        , activeBorderColor     = active
        , activeColor           = active
        , activeTextColor       = active
        , urgentBorderColor     = activeWarn
        , urgentTextColor       = yellow
        , decoHeight            = topbar
   }

-- addTopBar =  noFrillsDeco shrinkText topBarTheme

myTheme :: Theme
myTheme = def
  { activeColor = "#ffffff"
    , inactiveColor = "#000000"
    , urgentColor = "#FFFF00"
    , activeBorderColor = "#FFFFFF"
    , inactiveBorderColor = "#BBBBBB"
    , urgentBorderColor = "##00FF00"
    , activeTextColor = "#FFFFFF"
    , inactiveTextColor = "#BFBFBF"
    , urgentTextColor = "#FF0000"
    , fontName = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
    , decoWidth = 20
    , decoHeight = 20
    , windowTitleAddons = []
    , windowTitleIcons = []
    }

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask
altMask = mod1Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask, xK_Return),
     spawn $ XMonad.terminal conf)
  -- Copy window to all workspaces
  , ((modMask, xK_v),
     windows copyToAll)
  -- Remove copies 
  , ((modMask .|. shiftMask, xK_v),
      killAllOtherCopies)
  -- Lock the screen using command specified by myScreensaver.
  , ((modMask, xK_0),
     spawn myScreensaver)

  -- Spawn the launcher using command specified by myLauncher.
  -- Use this to launch programs without a key binding.
  , ((modMask .|. shiftMask, xK_Return),
     spawn myLauncher)

  -- Take a selective screenshot using the command specified by mySelectScreenshot.
  , ((modMask .|. shiftMask, xK_p),
     spawn mySelectScreenshot)

  -- Take a full screenshot using the command specified by myScreenshot.
  , ((modMask .|. controlMask .|. shiftMask, xK_p),
     spawn myScreenshot)

  -- Brightness up 
  , ((0, xF86XK_MonBrightnessUp),
     spawn brightnessUp)

  , ((0, xF86XK_MonBrightnessDown),
     spawn brightnessDown)
     -- spawn scratchpad
  , ((modMask, xK_a),
   namedScratchpadAction myScratchPads "terminal")
     
     -- Spawn mixer 
    , ((modMask .|. shiftMask, xK_z),
    namedScratchpadAction myScratchPads "pavucontrol")

    , ((modMask, xK_z),
    spawn browser)

    , ((modMask, xK_c),
    namedScratchpadAction myScratchPads "weechat")

    , ((modMask, xK_x),
    spawn "st lfrun")

    , ((modMask, xK_grave),
    spawn "dmenuunicode")

    , ((modMask, xK_w),
    spawn "bmks")

    , ((modMask, xK_F7),
    spawn "displayselect")
  -- Mute volume.
  , ((0, xF86XK_AudioMute),
     spawn "pulsemixer --toggle-mute")

  -- Decrease volume.
  , ((0, xF86XK_AudioLowerVolume),
     spawn "pulsemixer --change-volume -2")

  -- Increase volume.
  , ((0, xF86XK_AudioRaiseVolume),
     spawn "pulsemixer --change-volume +2")

  -- Audio previous.
  , ((0, 0x1008FF16),
     spawn "strawberry --previous")

  -- Play/pause.
  , ((0, 0x1008FF14),
     spawn "strawberry --play-pause")

  -- Audio next.
  , ((0, 0x1008FF17),
     spawn "strawberry --next")
  -- System actions 
  , ((modMask, xK_BackSpace),
     spawn "sysact")
  -- Eject CD tray.
--  , ((0, 0x1008FF2C),
----     spawn "eject -T")

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Close focused window.
  , ((modMask, xK_q),
     kill1)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  --, ((modMask .|. shiftMask, xK_space),
  --   setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  --, ((modMask, xK_comma),
   --  sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  --, ((modMask, xK_period),
  --   sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     io (exitWith ExitSuccess))

  -- Restart xmonad.
  , ((modMask, xK_r),
     restart "xmonad" True)
  ]
  
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ onCurrentScreen f i)
      | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]


  ++

  -- mod-{u,i,o}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{u,i,o}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_u, xK_i, xK_o] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
--
-- By default, do nothing.
myStartupHook = do
  setWMName "Xmonad"
  setDefaultCursor xC_left_ptr


------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
main = do
  nScreens <- countScreens
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc.hs"
  xmonad $ docks 
         $ defaults {
         logHook = dynamicLogWithPP xmobarPP {
                 ppSep = "   "
                , ppOutput = hPutStrLn xmproc
         } >> updatePointer (0.75, 0.75) (0.75, 0.75)
      }



------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    handleEventHook    = docksEventHook <+> handleEventHook def,
    layoutHook         = myLayout,
    manageHook         = manageDocks <+> myManageHook,
    startupHook        = myStartupHook
}
