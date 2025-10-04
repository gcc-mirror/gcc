# Enhanced VSCode Shell Integration Fix Script with Blackbox AI Support
# This script resolves VSCode shell integration warnings and optimizes for Blackbox AI

param(
    [switch]$Force,
    [switch]$Help,
    [switch]$Verbose
)

function Write-Log {
    param(
        [string]$Message,
        [string]$Level = "Info"
    )
    
    switch ($Level) {
        "Error" { Write-Host "ERROR: $Message" -ForegroundColor Red }
        "Warning" { Write-Host "WARNING: $Message" -ForegroundColor Yellow }
        "Success" { Write-Host "✓ $Message" -ForegroundColor Green }
        "Verbose" { 
            if ($Verbose) { Write-Host "VERBOSE: $Message" -ForegroundColor Gray }
        }
        default { Write-Host $Message -ForegroundColor White }
    }
}

if ($Help) {
    Write-Host @"
Enhanced VSCode Shell Integration Fix Script with Blackbox AI Support

DESCRIPTION:
    Fixes VSCode shell integration warnings for cmd.exe on Windows.
    Optimizes settings for Blackbox AI extension and improves error handling.
    Resolves errors like: "Shell integration cannot be enabled for executable cmd.exe"

USAGE:
    .\fix_vscode_shell_integration.ps1 [-Force] [-Help] [-Verbose]

PARAMETERS:
    -Force      Force overwrite existing VSCode settings
    -Help       Show this help message
    -Verbose    Enable detailed logging

EXAMPLES:
    .\fix_vscode_shell_integration.ps1
    .\fix_vscode_shell_integration.ps1 -Force -Verbose

FEATURES:
    • Enhanced VSCode detection (including Insiders)
    • Blackbox AI optimization settings
    • Improved error handling and validation
    • Better terminal integration for AI workflows
    • Automatic backup before changes
"@
    exit 0
}

Write-Host "Enhanced VSCode Shell Integration Fix with Blackbox AI Support" -ForegroundColor Green
Write-Host "=============================================================" -ForegroundColor Green

# Enhanced VSCode detection including Insiders
$vscodeInstalled = $false
$vscodePath = ""
$isInsiders = $false

$vscodePaths = @(
    # Standard VSCode
    @{ Path = "${env:LOCALAPPDATA}\Programs\Microsoft VS Code\Code.exe"; Type = "Standard" },
    @{ Path = "${env:PROGRAMFILES}\Microsoft VS Code\Code.exe"; Type = "Standard" },
    @{ Path = "${env:PROGRAMFILES(X86)}\Microsoft VS Code\Code.exe"; Type = "Standard" },
    # VSCode Insiders
    @{ Path = "${env:LOCALAPPDATA}\Programs\Microsoft VS Code Insiders\Code - Insiders.exe"; Type = "Insiders" },
    @{ Path = "${env:PROGRAMFILES}\Microsoft VS Code Insiders\Code - Insiders.exe"; Type = "Insiders" },
    @{ Path = "${env:PROGRAMFILES(X86)}\Microsoft VS Code Insiders\Code - Insiders.exe"; Type = "Insiders" }
)

foreach ($vscode in $vscodePaths) {
    if (Test-Path $vscode.Path) {
        $vscodeInstalled = $true
        $vscodePath = $vscode.Path
        $isInsiders = $vscode.Type -eq "Insiders"
        Write-Log "Found VSCode $($vscode.Type) at: $($vscode.Path)" -Level "Success"
        break
    }
}

if (-not $vscodeInstalled) {
    Write-Log "VSCode not found in standard installation paths." -Level "Error"
    Write-Log "Please ensure VSCode is installed before running this script." -Level "Error"
    exit 1
}

# Get VSCode settings paths (handle both standard and Insiders)
$appDataFolder = if ($isInsiders) { "Code - Insiders" } else { "Code" }
$vscodeUserSettings = "$env:APPDATA\$appDataFolder\User\settings.json"
$workspaceSettings = ".\.vscode\settings.json"

Write-Log "Configuring VSCode settings for Blackbox AI optimization..." -Level "Info"

# Create backup timestamp
$backupTimestamp = Get-Date -Format "yyyyMMdd_HHmmss"

# Create backup of existing settings
if (Test-Path $vscodeUserSettings) {
    $userBackup = "$env:APPDATA\$appDataFolder\User\settings_backup_$backupTimestamp.json"
    try {
        Copy-Item $vscodeUserSettings $userBackup -Force
        Write-Log "Created backup of user settings: $userBackup" -Level "Success"
    }
    catch {
        Write-Log "Could not create backup of user settings: $($_.Exception.Message)" -Level "Warning"
    }
}

if (Test-Path $workspaceSettings) {
    $workspaceBackup = ".\.vscode\settings_backup_$backupTimestamp.json"
    try {
        Copy-Item $workspaceSettings $workspaceBackup -Force
        Write-Log "Created backup of workspace settings: $workspaceBackup" -Level "Success"
    }
    catch {
        Write-Log "Could not create backup of workspace settings: $($_.Exception.Message)" -Level "Warning"
    }
}

# Create .vscode directory if it doesn't exist
if (-not (Test-Path ".\.vscode")) {
    try {
        New-Item -ItemType Directory -Path ".\.vscode" -Force | Out-Null
        Write-Log "Created .vscode directory" -Level "Success"
    }
    catch {
        Write-Log "Failed to create .vscode directory: $($_.Exception.Message)" -Level "Error"
        exit 1
    }
}

# Enhanced workspace settings configuration with Blackbox AI optimization
$workspaceConfig = @{
    # Core terminal integration
    "terminal.integrated.shellIntegration.enabled" = $true
    "terminal.integrated.shellIntegration.showWelcome" = $false
    "terminal.integrated.shellIntegration.decorationsEnabled" = $true
    "terminal.integrated.defaultProfile.windows" = "Command Prompt"
    
    # Enhanced terminal profiles
    "terminal.integrated.profiles.windows" = @{
        "Command Prompt" = @{
            "path" = @("${env:WINDIR}\System32\cmd.exe")
            "args" = @()
            "icon" = "terminal-cmd"
            "color" = "terminal.ansiBlue"
        }
        "PowerShell" = @{
            "source" = "PowerShell"
            "icon" = "terminal-powershell"
            "color" = "terminal.ansiBlue"
        }
        "Git Bash" = @{
            "source" = "Git Bash"
            "icon" = "terminal-bash"
        }
    }
    
    # Automation profile for AI tasks
    "terminal.integrated.automationProfile.windows" = @{
        "path" = "${env:WINDIR}\System32\cmd.exe"
        "args" = @()
    }
    
    # Blackbox AI optimization settings
    "blackbox.enabled" = $true
    "blackbox.autoComplete" = $true
    "blackbox.codeCompletion" = $true
    
    # Enhanced editor settings for AI assistance
    "editor.inlineSuggest.enabled" = $true
    "editor.suggestOnTriggerCharacters" = $true
    "editor.quickSuggestions" = @{
        "other" = $true
        "comments" = $true
        "strings" = $true
    }
    "editor.acceptSuggestionOnCommitCharacter" = $true
    "editor.acceptSuggestionOnEnter" = "on"
    "editor.tabCompletion" = "on"
    "editor.parameterHints.enabled" = $true
    "editor.hover.enabled" = $true
    
    # AI-friendly workspace settings
    "files.autoSave" = "afterDelay"
    "files.autoSaveDelay" = 1000
    "editor.formatOnSave" = $true
    "editor.codeActionsOnSave" = @{
        "source.organizeImports" = $true
        "source.fixAll" = $true
    }
}

# Write workspace settings with error handling
try {
    $workspaceJson = $workspaceConfig | ConvertTo-Json -Depth 10
    
    # Validate JSON before writing
    try {
        $workspaceJson | ConvertFrom-Json | Out-Null
    }
    catch {
        Write-Log "Generated workspace JSON is invalid: $($_.Exception.Message)" -Level "Error"
        exit 1
    }
    
    $workspaceJson | Out-File -FilePath $workspaceSettings -Encoding UTF8 -Force
    
    # Verify file was written correctly
    if (Test-Path $workspaceSettings) {
        Write-Log "Created workspace settings: $workspaceSettings" -Level "Success"
    } else {
        throw "Settings file was not created"
    }
}
catch {
    Write-Log "Failed to create workspace settings: $($_.Exception.Message)" -Level "Error"
    exit 1
}

# Enhanced user settings configuration with better logic
$shouldUpdateUserSettings = $Force -or -not (Test-Path $vscodeUserSettings)

if ($shouldUpdateUserSettings) {
    Write-Log "Updating user settings for Blackbox AI..." -Level "Verbose"
    
    # Minimal user settings (less intrusive than workspace settings)
    $userConfig = @{
        "terminal.integrated.shellIntegration.enabled" = $true
        "terminal.integrated.shellIntegration.showWelcome" = $false
        "terminal.integrated.defaultProfile.windows" = "Command Prompt"
        "blackbox.enabled" = $true
        "editor.inlineSuggest.enabled" = $true
    }
    
    # Ensure user settings directory exists
    $userSettingsDir = Split-Path $vscodeUserSettings -Parent
    if (-not (Test-Path $userSettingsDir)) {
        try {
            New-Item -ItemType Directory -Path $userSettingsDir -Force | Out-Null
            Write-Log "Created user settings directory: $userSettingsDir" -Level "Success"
        }
        catch {
            Write-Log "Failed to create user settings directory: $($_.Exception.Message)" -Level "Error"
        }
    }
    
    if (Test-Path $vscodeUserSettings) {
        # Merge with existing settings
        try {
            $existingContent = Get-Content $vscodeUserSettings -Raw
            if ($existingContent.Trim()) {
                $existingSettings = $existingContent | ConvertFrom-Json
                foreach ($key in $userConfig.Keys) {
                    $existingSettings | Add-Member -MemberType NoteProperty -Name $key -Value $userConfig[$key] -Force
                }
                $userConfig = $existingSettings
                Write-Log "Merged with existing user settings" -Level "Verbose"
            }
        }
        catch {
            Write-Log "Could not parse existing user settings. Creating new settings." -Level "Warning"
        }
    }
    
    try {
        $userJson = $userConfig | ConvertTo-Json -Depth 10
        
        # Validate JSON before writing
        try {
            $userJson | ConvertFrom-Json | Out-Null
        }
        catch {
            Write-Log "Generated user JSON is invalid: $($_.Exception.Message)" -Level "Error"
            return
        }
        
        $userJson | Out-File -FilePath $vscodeUserSettings -Encoding UTF8 -Force
        
        if (Test-Path $vscodeUserSettings) {
            Write-Log "Updated user settings: $vscodeUserSettings" -Level "Success"
        } else {
            Write-Log "User settings file was not created" -Level "Warning"
        }
    }
    catch {
        Write-Log "Failed to update user settings: $($_.Exception.Message)" -Level "Error"
    }
} else {
    Write-Log "User settings exist and -Force not specified. Skipping user settings update." -Level "Verbose"
    Write-Log "Use -Force to update existing user settings." -Level "Info"
}

# Create enhanced batch script with Blackbox AI support
$enhancedBatchScript = @'
@echo off
title VSCode Blackbox AI Integration Fix
echo ========================================
echo VSCode Blackbox AI Integration Fix
echo ========================================
echo.

REM Check if VSCode is running
tasklist /FI "IMAGENAME eq Code.exe" 2>NUL | find /I /N "Code.exe">NUL
if "%ERRORLEVEL%"=="0" (
    echo [INFO] VSCode is currently running
    choice /C YN /M "Close VSCode to apply settings"
    if errorlevel 2 goto :skip_close
    echo [INFO] Closing VSCode...
    taskkill /F /IM Code.exe >NUL 2>&1
    timeout /t 3 >NUL
)

:skip_close
REM Create .vscode directory
if not exist ".vscode" (
    mkdir .vscode
    echo [SUCCESS] Created .vscode directory
)

REM Create Blackbox AI optimized settings
echo [INFO] Creating Blackbox AI optimized settings...
(
echo {
echo   "terminal.integrated.shellIntegration.enabled": true,
echo   "terminal.integrated.shellIntegration.showWelcome": false,
echo   "terminal.integrated.defaultProfile.windows": "Command Prompt",
echo   "blackbox.enabled": true,
echo   "blackbox.autoComplete": true,
echo   "editor.inlineSuggest.enabled": true,
echo   "editor.quickSuggestions": {
echo     "other": true,
echo     "comments": true,
echo     "strings": true
echo   },
echo   "terminal.integrated.profiles.windows": {
echo     "Command Prompt": {
echo       "path": ["%WINDIR%\\System32\\cmd.exe"],
echo       "args": [],
echo       "icon": "terminal-cmd"
echo     }
echo   }
echo }
) > .vscode\settings.json

echo [SUCCESS] Blackbox AI optimized settings created!
echo.
echo Next steps:
echo 1. Restart VSCode
echo 2. Install Blackbox AI extension if not installed
echo 3. Open terminal (Ctrl+Shift+`)
echo 4. Test AI features
echo.
pause
'@

try {
    $enhancedBatchScript | Out-File -FilePath "fix_blackbox_integration.bat" -Encoding ASCII -Force
    Write-Log "Created enhanced batch script: fix_blackbox_integration.bat" -Level "Success"
}
catch {
    Write-Log "Failed to create batch script: $($_.Exception.Message)" -Level "Warning"
}

# Additional troubleshooting steps
Write-Host "`nAdditional Troubleshooting Steps:" -ForegroundColor Yellow
Write-Host "1. Restart VSCode completely" -ForegroundColor White
Write-Host "2. Open a new terminal in VSCode (Ctrl+Shift+`)" -ForegroundColor White
Write-Host "3. If issues persist, try running VSCode as Administrator" -ForegroundColor White
Write-Host "4. Check VSCode version (Help > About) - ensure it's up to date" -ForegroundColor White

# Check for common issues
Write-Host "`nChecking for common issues..." -ForegroundColor Yellow

# Check Windows version
$winVersion = [System.Environment]::OSVersion.Version
if ($winVersion.Major -lt 10) {
    Write-Warning "Windows version may not fully support shell integration features."
}

# Check if cmd.exe exists and is accessible
if (Test-Path "${env:WINDIR}\System32\cmd.exe") {
    Write-Host "✓ cmd.exe found and accessible" -ForegroundColor Green
} else {
    Write-Warning "cmd.exe not found at expected location"
}

# Final summary and completion
Write-Host "`n" + "="*50 -ForegroundColor Cyan
Write-Host "BLACKBOX AI VSCODE OPTIMIZATION COMPLETE" -ForegroundColor Cyan
Write-Host "="*50 -ForegroundColor Cyan

Write-Log "VSCode Shell Integration Fix with Blackbox AI optimization completed successfully!" -Level "Success"

Write-Host "`nOptimizations Applied:" -ForegroundColor Yellow
Write-Host "  • Enhanced terminal integration" -ForegroundColor Green
Write-Host "  • Blackbox AI extension settings" -ForegroundColor Green
Write-Host "  • AI-optimized editor configuration" -ForegroundColor Green
Write-Host "  • Improved IntelliSense settings" -ForegroundColor Green
Write-Host "  • Auto-save and formatting optimization" -ForegroundColor Green

Write-Host "`nNext Steps:" -ForegroundColor Yellow
Write-Host "  1. Restart VSCode completely" -ForegroundColor White
Write-Host "  2. Install Blackbox AI extension if not already installed" -ForegroundColor White
Write-Host "  3. Open a new terminal (Ctrl+Shift+`)" -ForegroundColor White
Write-Host "  4. Test AI code completion and chat features" -ForegroundColor White

if (Test-Path ".\.vscode\settings_backup_$backupTimestamp.json") {
    Write-Host "`nBackup Information:" -ForegroundColor Yellow
    Write-Host "  • Workspace backup: .\.vscode\settings_backup_$backupTimestamp.json" -ForegroundColor Cyan
}

# Offer to open VSCode
Write-Host ""
$openVSCode = Read-Host "Would you like to open VSCode now to test the Blackbox AI integration? (y/n)"
if ($openVSCode -eq 'y' -or $openVSCode -eq 'Y') {
    if ($vscodeInstalled) {
        Write-Log "Opening VSCode..." -Level "Success"
        try {
            Start-Process -FilePath $vscodePath -ArgumentList "." -ErrorAction Stop
        }
        catch {
            Write-Log "Could not open VSCode automatically. Please open it manually." -Level "Warning"
        }
    }
}

Write-Host "`n🚀 Happy coding with Blackbox AI! 🚀" -ForegroundColor Green
