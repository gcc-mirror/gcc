@echo off
if %1.==go32. goto call_go32
if %1.==winnt. goto call_winnt
echo Usage: configure go32 or configure winnt cpu
goto END

:call_go32
call config\msdos\configure %1 %2 %3 %4
goto END

:call_winnt
call config\%2\config-nt %1 %2 %3 %4
goto END

:END

