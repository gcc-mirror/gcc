@echo off
if %1.==msdos. goto call_msdos
if %1.==winnt. goto call_winnt
echo Usage: configure msdos or configure winnt
goto END

:call_msdos
call config\msdos\configure %1 %2 %3 %4
goto END

:call_winnt
call config\i386\config-nt %1 %2 %3 %4
goto END

:END
