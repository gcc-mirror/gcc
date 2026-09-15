# Enhanced VSCode Shell Integration Fix Script with Blackbox AI Optimization
# This script resolves VSCode shell integration warnings and optimizes settings for Blackbox AI

param(
    [switch]$Force,
    [switch]$Help,
    [switch]$BackupOnly,
    [switch]$Restore,
    [string]$BackupPath = "",
    [switch]$Verbose
)

# Global variables
$script:LogLevel = if ($Verbose) { "Verbose" } else { "Normal" }
$script:BackupTimestamp = Get-Date -Format "yyyyMMdd_HHmmss"

function Write-Log {
    param(
        [string]$Message,
        [string]$Level = "Info",
        [string]$Color = "White"
    )
    
    $timestamp = Get-Date -Format "HH:mm:ss"
    
    switch ($Level) {
        "Error" { Write-Host "[$timestamp] ERROR: $Message" -ForegroundColor Red }
        "Warning" { Write-Host "[$timestamp] WARNING: $Message" -ForegroundColor Yellow }
        "Success" { Write-Host "[$timestamp] SUCCESS: $Message" -ForegroundColor Green }
        "Verbose" { 
            if ($script:LogLevel -eq "Verbose") {
                Write-Host "[$timestamp] VERBOSE: $Message" -ForegroundColor Gray
            }
        }
        default { Write-Host "[$timestamp] $Message" -ForegroundColor $Color }
    }
}

function Show-Help {
    Write-Host @"
Enhanced VSCode Shell Integration Fix Script with Blackbox AI Optimization

DESCRIPTION:
    Fixes VSCode shell integration warnings for cmd.exe on Windows and optimizes
    settings for Blackbox AI extension. Includes backup/restore functionality.

USAGE:
    .\fix_vscode_shell_integration_enhanced.ps1 [OPTIONS]

PARAMETERS:
    -Force          Force overwrite existing VSCode settings
    -Help           Show this help message
    -BackupOnly     Only create backup of current settings
    -Restore        Restore from backup (use with -BackupPath)
    -BackupPath     Specify custom backup path for restore
    -Verbose        Enable verbose logging

EXAMPLES:
    .\fix_vscode_shell_integration_enhanced.ps1
    .\fix_vscode_shell_integration_enhanced.ps1 -Force -Verbose
    .\fix_vscode_shell_integration_enhanced.ps1 -BackupOnly
    .\fix_vscode_shell_integration_enhanced.ps1 -Restore -BackupPath "backup_20231201_143022"

FEATURES:
    • Enhanced VSCode detection (including Insiders)
    • Blackbox AI optimization settings
    • Automatic backup before changes
    • Improved error handling and validation
    • Better terminal integration for AI workflows
    • Rollback functionality
"@
}

function Test-JsonValid {
    param([string]$JsonString)
    
    try {
        $JsonString | ConvertFrom-Json | Out-Null
        return $true
    }
    catch {
        return $false
    }
}

function Get-VSCodeInstallations {
    Write-Log "Detecting VSCode installations..." -Level "Verbose"
    
    $vscodeInstallations = @()
    $searchPaths = @(
        # Standard VSCode
        "${env:LOCALAPPDATA}\Programs\Microsoft VS Code\Code.exe",
        "${env:PROGRAMFILES}\Microsoft VS Code\Code.exe",
        "${env:PROGRAMFILES(X86)}\Microsoft VS Code\Code.exe",
        # VSCode Insiders
        "${env:LOCALAPPDATA}\Programs\Microsoft VS Code Insiders\Code - Insiders.exe",
        "${env:PROGRAMFILES}\Microsoft VS Code Insiders\Code - Insiders.exe",
        "${env:PROGRAMFILES(X86)}\Microsoft VS Code Insiders\Code - Insiders.exe",
        # Portable installations
        ".\Code.exe",
        ".\VSCode-win32-x64\Code.exe"
    )
    
    foreach ($path in $searchPaths) {
        if (Test-Path $path) {
            $version = "Unknown"
            $isInsiders = $path -like "*Insiders*"
            
            try {
                $versionInfo = Get-ItemProperty $path | Select-Object -ExpandProperty VersionInfo
                $version = $versionInfo.ProductVersion
            }
            catch {
                Write-Log "Could not determine version for: $path" -Level "Verbose"
            }
            
            $installation = @{
                Path = $path
                Version = $version
                IsInsiders = $isInsiders
                Type = if ($isInsiders) { "Insiders" } else { "Stable" }
            }
            
            $vscodeInstallations += $installation
            Write-Log "Found VSCode $($installation.Type) at: $path (v$version)" -Level "Success"
        }
    }
    
    return $vscodeInstallations
}

function Get-VSCodeSettingsPaths {
    param([bool]$IsInsiders = $false)
    
    $appDataFolder = if ($IsInsiders) { "Code - Insiders" } else { "Code" }
    
    return @{
        UserSettings = "$env:APPDATA\$appDataFolder\User\settings.json"
        WorkspaceSettings = ".\.vscode\settings.json"
        UserDir = "$env:APPDATA\$appDataFolder\User"
        WorkspaceDir = ".\.vscode"
    }
}

function Backup-VSCodeSettings {
    param(
        [hashtable]$SettingsPaths,
        [string]$BackupDir = "vscode_backup_$script:BackupTimestamp"
    )
    
    Write-Log "Creating backup in: $BackupDir"
    
    try {
        if (-not (Test-Path $BackupDir)) {
            New-Item -ItemType Directory -Path $BackupDir -Force | Out-Null
        }
        
        $backupInfo = @{
            Timestamp = $script:BackupTimestamp
            Files = @()
        }
        
        # Backup user settings
        if (Test-Path $SettingsPaths.UserSettings) {
            $userBackupPath = Join-Path $BackupDir "user_settings.json"
            Copy-Item $SettingsPaths.UserSettings $userBackupPath -Force
            $backupInfo.Files += @{ Original = $SettingsPaths.UserSettings; Backup = $userBackupPath }
            Write-Log "Backed up user settings" -Level "Success"
        }
        
        # Backup workspace settings
        if (Test-Path $SettingsPaths.WorkspaceSettings) {
            $workspaceBackupPath = Join-Path $BackupDir "workspace_settings.json"
            Copy-Item $SettingsPaths.WorkspaceSettings $workspaceBackupPath -Force
            $backupInfo.Files += @{ Original = $SettingsPaths.WorkspaceSettings; Backup = $workspaceBackupPath }
            Write-Log "Backed up workspace settings" -Level "Success"
        }
        
        # Save backup info
        $backupInfoPath = Join-Path $BackupDir "backup_info.json"
        $backupInfo | ConvertTo-Json -Depth 10 | Out-File $backupInfoPath -Encoding UTF8
        
        Write-Log "Backup completed successfully: $BackupDir" -Level "Success"
        return $BackupDir
    }
    catch {
        Write-Log "Backup failed: $($_.Exception.Message)" -Level "Error"
        return $null
    }
}

function Restore-VSCodeSettings {
    param([string]$BackupPath)
    
    if (-not (Test-Path $BackupPath)) {
        Write-Log "Backup path not found: $BackupPath" -Level "Error"
        return $false
    }
    
    $backupInfoPath = Join-Path $BackupPath "backup_info.json"
    if (-not (Test-Path $backupInfoPath)) {
        Write-Log "Backup info file not found: $backupInfoPath" -Level "Error"
        return $false
    }
    
    try {
        $backupInfo = Get-Content $backupInfoPath -Raw | ConvertFrom-Json
        
        foreach ($fileInfo in $backupInfo.Files) {
            if (Test-Path $fileInfo.Backup) {
                # Ensure target directory exists
                $targetDir = Split-Path $fileInfo.Original -Parent
                if (-not (Test-Path $targetDir)) {
                    New-Item -ItemType Directory -Path $targetDir -Force | Out-Null
                }
                
                Copy-Item $fileInfo.Backup $fileInfo.Original -Force
                Write-Log "Restored: $($fileInfo.Original)" -Level "Success"
            }
        }
        
        Write-Log "Settings restored successfully from backup: $BackupPath" -Level "Success"
        return $true
    }
    catch {
        Write-Log "Restore failed: $($_.Exception.Message)" -Level "Error"
        return $false
    }
}

function Get-BlackboxOptimizedSettings {
    return @{
        # Blackbox AI Extension Settings
        "blackbox.enabled" = $true
        "blackbox.autoComplete" = $true
        "blackbox.codeCompletion" = $true
        "blackbox.chatEnabled" = $true
        
        # Terminal Integration for AI Workflows
        "terminal.integrated.shellIntegration.enabled" = $true
        "terminal.integrated.shellIntegration.showWelcome" = $false
        "terminal.integrated.shellIntegration.decorationsEnabled" = $true
        "terminal.integrated.defaultProfile.windows" = "Command Prompt"
        
        # Enhanced Terminal Profiles
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
            "Developer Command Prompt" = @{
                "path" = @("${env:WINDIR}\System32\cmd.exe")
                "args" = @("/k", "C:\Program Files\Microsoft Visual Studio\2022\Community\Common7\Tools\VsDevCmd.bat")
                "icon" = "terminal-cmd"
            }
        }
        
        # Automation Profile for AI Tasks
        "terminal.integrated.automationProfile.windows" = @{
            "path" = "${env:WINDIR}\System32\cmd.exe"
            "args" = @()
        }
        
        # Editor Settings for AI Assistance
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
        "editor.wordBasedSuggestions" = $true
        "editor.parameterHints.enabled" = $true
        "editor.hover.enabled" = $true
        
        # IntelliSense Optimization
        "typescript.suggest.autoImports" = $true
        "javascript.suggest.autoImports" = $true
        "python.analysis.autoImportCompletions" = $true
        
        # AI-Friendly Workspace Settings
        "files.autoSave" = "afterDelay"
        "files.autoSaveDelay" = 1000
        "editor.formatOnSave" = $true
        "editor.formatOnPaste" = $true
        "editor.codeActionsOnSave" = @{
            "source.organizeImports" = $true
            "source.fixAll" = $true
        }
        
        # Enhanced Git Integration for AI Workflows
        "git.enableSmartCommit" = $true
        "git.autofetch" = $true
        "git.confirmSync" = $false
        
        # Performance Settings for AI Extensions
        "extensions.autoUpdate" = $true
        "update.mode" = "start"
        "telemetry.telemetryLevel" = "error"
    }
}

function Merge-Settings {
    param(
        [hashtable]$ExistingSettings,
        [hashtable]$NewSettings
    )
    
    $merged = $ExistingSettings.Clone()
    
    foreach ($key in $NewSettings.Keys) {
        $merged[$key] = $NewSettings[$key]
        Write-Log "Updated setting: $key" -Level "Verbose"
    }
    
    return $merged
}

function Update-VSCodeSettings {
    param(
        [hashtable]$SettingsPaths,
        [hashtable]$NewSettings,
        [string]$SettingsType = "workspace"
    )
    
    $settingsFile = if ($SettingsType -eq "user") { $SettingsPaths.UserSettings } else { $SettingsPaths.WorkspaceSettings }
    $settingsDir = Split-Path $settingsFile -Parent
    
    Write-Log "Updating $SettingsType settings: $settingsFile"
    
    try {
        # Ensure directory exists
        if (-not (Test-Path $settingsDir)) {
            New-Item -ItemType Directory -Path $settingsDir -Force | Out-Null
            Write-Log "Created directory: $settingsDir" -Level "Success"
        }
        
        $finalSettings = $NewSettings
        
        # Merge with existing settings if file exists
        if (Test-Path $settingsFile) {
            try {
                $existingContent = Get-Content $settingsFile -Raw -ErrorAction Stop
                if ($existingContent.Trim()) {
                    $existingSettings = $existingContent | ConvertFrom-Json -AsHashtable -ErrorAction Stop
                    $finalSettings = Merge-Settings -ExistingSettings $existingSettings -NewSettings $NewSettings
                    Write-Log "Merged with existing $SettingsType settings" -Level "Verbose"
                }
            }
            catch {
                Write-Log "Could not parse existing $SettingsType settings, creating new file" -Level "Warning"
            }
        }
        
        # Convert to JSON and validate
        $jsonContent = $finalSettings | ConvertTo-Json -Depth 10
        if (-not (Test-JsonValid -JsonString $jsonContent)) {
            throw "Generated JSON is invalid"
        }
        
        # Write settings file
        $jsonContent | Out-File -FilePath $settingsFile -Encoding UTF8 -Force
        
        # Verify file was written correctly
        if (Test-Path $settingsFile) {
            $verifyContent = Get-Content $settingsFile -Raw
            if (Test-JsonValid -JsonString $verifyContent) {
                Write-Log "Successfully updated $SettingsType settings" -Level "Success"
                return $true
            } else {
                throw "Written file contains invalid JSON"
            }
        } else {
            throw "Settings file was not created"
        }
    }
    catch {
        Write-Log "Failed to update $SettingsType settings: $($_.Exception.Message)" -Level "Error"
        return $false
    }
}

function Test-SystemRequirements {
    Write-Log "Checking system requirements..." -Level "Verbose"
    
    $issues = @()
    
    # Check Windows version
    $winVersion = [System.Environment]::OSVersion.Version
    if ($winVersion.Major -lt 10) {
        $issues += "Windows 10 or later is recommended for full shell integration support"
    }
    
    # Check PowerShell version
    if ($PSVersionTable.PSVersion.Major -lt 5) {
        $issues += "PowerShell 5.0 or later is required"
    }
    
    # Check cmd.exe accessibility
    if (-not (Test-Path "${env:WINDIR}\System32\cmd.exe")) {
        $issues += "cmd.exe not found at expected location"
    }
    
    # Check write permissions
    try {
        $testFile = Join-Path $env:TEMP "vscode_test_$(Get-Random).tmp"
        "test" | Out-File $testFile -Force
        Remove-Item $testFile -Force
    }
    catch {
        $issues += "Insufficient file system permissions"
    }
    
    if ($issues.Count -gt 0) {
        Write-Log "System requirement issues found:" -Level "Warning"
        foreach ($issue in $issues) {
            Write-Log "  • $issue" -Level "Warning"
        }
        return $false
    }
    
    Write-Log "System requirements check passed" -Level "Success"
    return $true
}

function Show-Summary {
    param(
        [array]$VSCodeInstallations,
        [string]$BackupPath,
        [bool]$WorkspaceUpdated,
        [bool]$UserUpdated
    )
    
    Write-Host "`n" + "="*60 -ForegroundColor Cyan
    Write-Host "BLACKBOX AI VSCODE OPTIMIZATION COMPLETE" -ForegroundColor Cyan
    Write-Host "="*60 -ForegroundColor Cyan
    
    Write-Host "`nVSCode Installations Found:" -ForegroundColor Yellow
    foreach ($installation in $VSCodeInstallations) {
        Write-Host "  • $($installation.Type) v$($installation.Version)" -ForegroundColor Green
        Write-Host "    Path: $($installation.Path)" -ForegroundColor Gray
    }
    
    if ($BackupPath) {
        Write-Host "`nBackup Created:" -ForegroundColor Yellow
        Write-Host "  • Location: $BackupPath" -ForegroundColor Green
    }
    
    Write-Host "`nSettings Updated:" -ForegroundColor Yellow
    Write-Host "  • Workspace Settings: $(if ($WorkspaceUpdated) { '✓ Updated' } else { '✗ Failed' })" -ForegroundColor $(if ($WorkspaceUpdated) { 'Green' } else { 'Red' })
    Write-Host "  • User Settings: $(if ($UserUpdated) { '✓ Updated' } else { '- Skipped' })" -ForegroundColor $(if ($UserUpdated) { 'Green' } else { 'Yellow' })
    
    Write-Host "`nBlackbox AI Optimizations Applied:" -ForegroundColor Yellow
    Write-Host "  • Enhanced terminal integration" -ForegroundColor Green
    Write-Host "  • AI-optimized editor settings" -ForegroundColor Green
    Write-Host "  • Improved IntelliSense configuration" -ForegroundColor Green
    Write-Host "  • Blackbox extension settings" -ForegroundColor Green
    Write-Host "  • Performance optimizations" -ForegroundColor Green
    
    Write-Host "`nNext Steps:" -ForegroundColor Yellow
    Write-Host "  1. Restart VSCode completely" -ForegroundColor White
    Write-Host "  2. Install/Enable Blackbox AI extension if not already done" -ForegroundColor White
    Write-Host "  3. Open a new terminal (Ctrl+Shift+`)" -ForegroundColor White
    Write-Host "  4. Test AI code completion and chat features" -ForegroundColor White
    
    if ($BackupPath) {
        Write-Host "`nRestore Command (if needed):" -ForegroundColor Yellow
        Write-Host "  .\fix_vscode_shell_integration_enhanced.ps1 -Restore -BackupPath `"$BackupPath`"" -ForegroundColor Cyan
    }
}

# Main execution
function Main {
    if ($Help) {
        Show-Help
        exit 0
    }
    
    Write-Host "Enhanced VSCode Shell Integration Fix with Blackbox AI Optimization" -ForegroundColor Green
    Write-Host "=================================================================" -ForegroundColor Green
    Write-Host "Version: 2.0 | Optimized for Blackbox AI | $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')" -ForegroundColor Gray
    Write-Host ""
    
    # Handle restore operation
    if ($Restore) {
        $restorePath = if ($BackupPath) { $BackupPath } else {
            Write-Host "Available backups:" -ForegroundColor Yellow
            Get-ChildItem -Directory -Name "vscode_backup_*" | ForEach-Object { Write-Host "  • $_" -ForegroundColor Cyan }
            Read-Host "Enter backup directory name"
        }
        
        if (Restore-VSCodeSettings -BackupPath $restorePath) {
            Write-Log "Restore completed successfully" -Level "Success"
        } else {
            Write-Log "Restore failed" -Level "Error"
            exit 1
        }
        exit 0
    }
    
    # System requirements check
    if (-not (Test-SystemRequirements)) {
        Write-Log "System requirements not met. Please address the issues above." -Level "Error"
        exit 1
    }
    
    # Detect VSCode installations
    $vscodeInstallations = Get-VSCodeInstallations
    if ($vscodeInstallations.Count -eq 0) {
        Write-Log "No VSCode installations found. Please install VSCode first." -Level "Error"
        exit 1
    }
    
    # Use the first (primary) installation for settings paths
    $primaryInstallation = $vscodeInstallations[0]
    $settingsPaths = Get-VSCodeSettingsPaths -IsInsiders $primaryInstallation.IsInsiders
    
    # Create backup (unless BackupOnly mode)
    $backupPath = $null
    if (-not $BackupOnly) {
        $backupPath = Backup-VSCodeSettings -SettingsPaths $settingsPaths
        if (-not $backupPath) {
            Write-Log "Backup failed. Aborting to prevent data loss." -Level "Error"
            exit 1
        }
    } else {
        $backupPath = Backup-VSCodeSettings -SettingsPaths $settingsPaths
        Write-Log "Backup-only mode completed" -Level "Success"
        exit 0
    }
    
    # Get optimized settings
    $optimizedSettings = Get-BlackboxOptimizedSettings
    
    # Update workspace settings
    Write-Log "Updating workspace settings for Blackbox AI optimization..."
    $workspaceUpdated = Update-VSCodeSettings -SettingsPaths $settingsPaths -NewSettings $optimizedSettings -SettingsType "workspace"
    
    # Update user settings (if Force is specified or user settings don't exist)
    $userUpdated = $false
    if ($Force -or -not (Test-Path $settingsPaths.UserSettings)) {
        Write-Log "Updating user settings for Blackbox AI optimization..."
        
        # Create minimal user settings (less intrusive than workspace settings)
        $userSettings = @{
            "terminal.integrated.shellIntegration.enabled" = $true
            "terminal.integrated.shellIntegration.showWelcome" = $false
            "blackbox.enabled" = $true
            "editor.inlineSuggest.enabled" = $true
        }
        
        $userUpdated = Update-VSCodeSettings -SettingsPaths $settingsPaths -NewSettings $userSettings -SettingsType "user"
    } else {
        Write-Log "User settings exist and -Force not specified. Skipping user settings update." -Level "Verbose"
    }
    
    # Create enhanced batch script
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

REM Create optimized settings for Blackbox AI
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
    
    $enhancedBatchScript | Out-File -FilePath "fix_blackbox_integration.bat" -Encoding ASCII -Force
    Write-Log "Created enhanced batch script: fix_blackbox_integration.bat" -Level "Success"
    
    # Show completion summary
    Show-Summary -VSCodeInstallations $vscodeInstallations -BackupPath $backupPath -WorkspaceUpdated $workspaceUpdated -UserUpdated $userUpdated
    
    # Offer to open VSCode
    $openVSCode = Read-Host "`nWould you like to open VSCode now to test the Blackbox AI integration? (y/n)"
    if ($openVSCode -eq 'y' -or $openVSCode -eq 'Y') {
        Write-Log "Opening VSCode..." -Level "Success"
        try {
            Start-Process -FilePath $primaryInstallation.Path -ArgumentList "." -ErrorAction Stop
        }
        catch {
            Write-Log "Could not open VSCode automatically. Please open it manually." -Level "Warning"
        }
    }
    
    Write-Host "`n🚀 Blackbox AI optimization complete! Happy coding! 🚀" -ForegroundColor Green
}

# Execute main function
try {
    Main
}
catch {
    Write-Log "Script execution failed: $($_.Exception.Message)" -Level "Error"
    Write-Log "Stack trace: $($_.ScriptStackTrace)" -Level "Verbose"
    exit 1
}
