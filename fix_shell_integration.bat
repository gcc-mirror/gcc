@echo off
setlocal enabledelayedexpansion

echo VSCode Shell Integration Fix (Batch Version)
echo ============================================
echo.

REM Check if running as administrator
net session >nul 2>&1
if %errorLevel% == 0 (
    echo Running as Administrator: YES
) else (
    echo Running as Administrator: NO
    echo Note: Some operations may require administrator privileges
)
echo.

REM Check if VSCode is running and offer to close it
tasklist /FI "IMAGENAME eq Code.exe" 2>NUL | find /I /N "Code.exe">NUL
if "%ERRORLEVEL%"=="0" (
    echo VSCode is currently running.
    set /p closeVSCode="Close VSCode to apply changes? (y/n): "
    if /i "!closeVSCode!"=="y" (
        echo Closing VSCode...
        taskkill /F /IM Code.exe >NUL 2>&1
        timeout /t 3 >NUL
        echo VSCode closed.
    )
)

REM Create .vscode directory if it doesn't exist
if not exist ".vscode" (
    mkdir .vscode
    echo Created .vscode directory
)

REM Backup existing settings if they exist
if exist ".vscode\settings.json" (
    copy ".vscode\settings.json" ".vscode\settings.json.backup" >NUL 2>&1
    echo Backed up existing settings to settings.json.backup
)

REM Create VSCode workspace settings
echo Creating VSCode workspace settings...
(
echo {
echo   "terminal.integrated.shellIntegration.enabled": true,
echo   "terminal.integrated.shellIntegration.showWelcome": false,
echo   "terminal.integrated.defaultProfile.windows": "Command Prompt",
echo   "terminal.integrated.profiles.windows": {
echo     "Command Prompt": {
echo       "path": ["%WINDIR%\\System32\\cmd.exe"],
echo       "args": [],
echo       "icon": "terminal-cmd"
echo     },
echo     "PowerShell": {
echo       "source": "PowerShell",
echo       "icon": "terminal-powershell"
echo     },
echo     "Git Bash": {
echo       "source": "Git Bash"
echo     }
echo   },
echo   "terminal.integrated.automationProfile.windows": {
echo     "path": "%WINDIR%\\System32\\cmd.exe",
echo     "args": []
echo   },
echo   "terminal.integrated.env.windows": {},
echo   "terminal.integrated.inheritEnv": true,
echo   "terminal.integrated.cwd": "${workspaceFolder}",
echo   "terminal.integrated.scrollback": 10000,
echo   "terminal.integrated.enableBell": false
echo }
) > .vscode\settings.json

if exist ".vscode\settings.json" (
    echo ✓ VSCode workspace settings created successfully!
) else (
    echo ✗ Failed to create VSCode settings file
    goto :error
)

REM Check for VSCode installation
set "vscode_found=0"
if exist "%LOCALAPPDATA%\Programs\Microsoft VS Code\Code.exe" (
    set "vscode_path=%LOCALAPPDATA%\Programs\Microsoft VS Code\Code.exe"
    set "vscode_found=1"
) else if exist "%PROGRAMFILES%\Microsoft VS Code\Code.exe" (
    set "vscode_path=%PROGRAMFILES%\Microsoft VS Code\Code.exe"
    set "vscode_found=1"
) else if exist "%PROGRAMFILES(X86)%\Microsoft VS Code\Code.exe" (
    set "vscode_path=%PROGRAMFILES(X86)%\Microsoft VS Code\Code.exe"
    set "vscode_found=1"
)

if !vscode_found! == 1 (
    echo ✓ VSCode found at: !vscode_path!
) else (
    echo ⚠ VSCode not found in standard installation paths
    echo   Please ensure VSCode is installed
)

REM Check cmd.exe accessibility
if exist "%WINDIR%\System32\cmd.exe" (
    echo ✓ cmd.exe found and accessible
) else (
    echo ✗ cmd.exe not found at expected location
    goto :error
)

echo.
echo ============================================
echo Configuration completed successfully!
echo ============================================
echo.
echo Next steps:
echo 1. Restart VSCode completely
echo 2. Open a new terminal in VSCode (Ctrl+Shift+`)
echo 3. Verify that shell integration warnings are gone
echo.
echo If issues persist:
echo - Try running VSCode as Administrator
echo - Check VSCode version (Help ^> About)
echo - Ensure VSCode is up to date
echo.

REM Offer to open VSCode
if !vscode_found! == 1 (
    set /p openVSCode="Open VSCode now? (y/n): "
    if /i "!openVSCode!"=="y" (
        echo Opening VSCode...
        start "" "!vscode_path!" .
    )
)

echo.
echo Press any key to exit...
pause >nul
goto :end

:error
echo.
echo ============================================
echo An error occurred during configuration
echo ============================================
echo Please check the error messages above and try again.
echo You may need to run this script as Administrator.
echo.
pause
exit /b 1

:end
exit /b 0
