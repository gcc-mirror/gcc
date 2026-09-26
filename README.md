# Enhanced VSCode Shell Integration Fix with Blackbox AI Optimization

This repository contains enhanced PowerShell scripts to fix VSCode shell integration warnings and optimize settings for Blackbox AI development workflows.

## 🚀 Features

### Core Improvements
- ✅ **Enhanced VSCode Detection**: Supports both standard VSCode and VSCode Insiders
- ✅ **Blackbox AI Optimization**: Specialized settings for optimal AI coding experience
- ✅ **Automatic Backup**: Creates timestamped backups before making changes
- ✅ **Improved Error Handling**: Comprehensive validation and error recovery
- ✅ **Better User Experience**: Enhanced logging and progress feedback
- ✅ **JSON Validation**: Ensures settings files are valid before writing

### Blackbox AI Optimizations
- 🤖 **AI Extension Settings**: Optimized configuration for Blackbox AI extension
- 🔧 **Enhanced IntelliSense**: Improved code completion and suggestions
- ⚡ **Performance Tuning**: Optimized settings for AI-powered development
- 🎯 **Smart Suggestions**: Enhanced quick suggestions for comments and strings
- 💾 **Auto-save Configuration**: Optimized file handling for AI workflows
- 🔄 **Format on Save**: Automatic code formatting and import organization

## 📁 Files

### Main Scripts
- **`fix_vscode_shell_integration.ps1`** - Enhanced main script with Blackbox AI support
- **`fix_vscode_shell_integration_enhanced.ps1`** - Full-featured version with advanced options
- **`fix_blackbox_integration.bat`** - Batch script alternative for quick fixes

### Documentation
- **`README.md`** - This comprehensive guide
- **`IMPROVEMENT_PLAN.md`** - Detailed improvement roadmap
- **`SHELL_INTEGRATION_FIX.md`** - Technical documentation

## 🛠️ Usage

### Quick Start
```powershell
# Basic usage - applies Blackbox AI optimizations
.\fix_vscode_shell_integration.ps1

# Force update existing settings
.\fix_vscode_shell_integration.ps1 -Force

# Enable verbose logging
.\fix_vscode_shell_integration.ps1 -Verbose -Force
```

### Advanced Usage (Enhanced Script)
```powershell
# Full optimization with verbose output
.\fix_vscode_shell_integration_enhanced.ps1 -Force -Verbose

# Create backup only (no changes)
.\fix_vscode_shell_integration_enhanced.ps1 -BackupOnly

# Restore from backup
.\fix_vscode_shell_integration_enhanced.ps1 -Restore -BackupPath "vscode_backup_20231201_143022"

# Show help
.\fix_vscode_shell_integration_enhanced.ps1 -Help
```

### Batch Script Alternative
```batch
# Run the batch script for quick setup
fix_blackbox_integration.bat
```

## ⚙️ Settings Applied

### Terminal Integration
```json
{
  "terminal.integrated.shellIntegration.enabled": true,
  "terminal.integrated.shellIntegration.showWelcome": false,
  "terminal.integrated.shellIntegration.decorationsEnabled": true,
  "terminal.integrated.defaultProfile.windows": "Command Prompt"
}
```

### Blackbox AI Optimization
```json
{
  "blackbox.enabled": true,
  "blackbox.autoComplete": true,
  "blackbox.codeCompletion": true,
  "blackbox.chatEnabled": true
}
```

### Enhanced Editor Settings
```json
{
  "editor.inlineSuggest.enabled": true,
  "editor.suggestOnTriggerCharacters": true,
  "editor.quickSuggestions": {
    "other": true,
    "comments": true,
    "strings": true
  },
  "editor.acceptSuggestionOnCommitCharacter": true,
  "editor.acceptSuggestionOnEnter": "on",
  "editor.tabCompletion": "on"
}
```

### AI-Friendly Workspace Settings
```json
{
  "files.autoSave": "afterDelay",
  "files.autoSaveDelay": 1000,
  "editor.formatOnSave": true,
  "editor.codeActionsOnSave": {
    "source.organizeImports": true,
    "source.fixAll": true
  }
}
```

## 🔧 Parameters

### Main Script Parameters
| Parameter | Description | Example |
|-----------|-------------|---------|
| `-Force` | Force overwrite existing settings | `.\script.ps1 -Force` |
| `-Help` | Show help information | `.\script.ps1 -Help` |
| `-Verbose` | Enable detailed logging | `.\script.ps1 -Verbose` |

### Enhanced Script Parameters
| Parameter | Description | Example |
|-----------|-------------|---------|
| `-BackupOnly` | Create backup without changes | `.\script.ps1 -BackupOnly` |
| `-Restore` | Restore from backup | `.\script.ps1 -Restore -BackupPath "backup_dir"` |
| `-BackupPath` | Specify backup directory | Used with `-Restore` |

## 🛡️ Safety Features

### Automatic Backups
- Creates timestamped backups before making changes
- Backup format: `settings_backup_YYYYMMDD_HHMMSS.json`
- Supports both user and workspace settings

### Validation
- JSON validation before writing settings
- File existence verification after creation
- Error handling with detailed messages

### Rollback Support
```powershell
# Restore from automatic backup
.\fix_vscode_shell_integration_enhanced.ps1 -Restore -BackupPath "vscode_backup_20231201_143022"
```

## 🎯 Supported VSCode Versions

### Standard VSCode
- `%LOCALAPPDATA%\Programs\Microsoft VS Code\Code.exe`
- `%PROGRAMFILES%\Microsoft VS Code\Code.exe`
- `%PROGRAMFILES(X86)%\Microsoft VS Code\Code.exe`

### VSCode Insiders
- `%LOCALAPPDATA%\Programs\Microsoft VS Code Insiders\Code - Insiders.exe`
- `%PROGRAMFILES%\Microsoft VS Code Insiders\Code - Insiders.exe`
- `%PROGRAMFILES(X86)%\Microsoft VS Code Insiders\Code - Insiders.exe`

## 🔍 Troubleshooting

### Common Issues

1. **Permission Errors**
   ```powershell
   # Run PowerShell as Administrator
   Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
   ```

2. **VSCode Not Found**
   - Ensure VSCode is installed in standard locations
   - Check if using portable installation
   - Verify installation path manually

3. **Settings Not Applied**
   - Restart VSCode completely
   - Check if Blackbox AI extension is installed
   - Verify settings files were created correctly

### Verification Steps
1. Open VSCode
2. Press `Ctrl+Shift+P` and type "Terminal: Create New Terminal"
3. Check if shell integration warnings are gone
4. Test Blackbox AI features (if extension installed)

## 📋 System Requirements

- **OS**: Windows 10 or later
- **PowerShell**: 5.0 or later
- **VSCode**: Any recent version
- **Permissions**: Write access to VSCode settings directories

## 🔄 What's New in Enhanced Version

### Version 2.0 Features
- ✨ **Blackbox AI Integration**: Specialized settings for AI development
- 🔒 **Enhanced Safety**: Automatic backups and rollback support
- 🎯 **Better Detection**: Support for VSCode Insiders and custom paths
- 📊 **Improved Logging**: Detailed progress and error reporting
- ⚡ **Performance**: Optimized settings for AI workflows
- 🛠️ **Advanced Options**: Backup-only and restore functionality

### Improvements Over Original
- Fixed user settings update logic
- Added comprehensive error handling
- Enhanced VSCode detection (including Insiders)
- Automatic backup creation
- JSON validation before writing
- Better merge logic for existing settings
- Improved user experience with detailed feedback

## 📞 Support

If you encounter issues:

1. **Check the logs**: Use `-Verbose` flag for detailed output
2. **Verify backups**: Automatic backups are created for safety
3. **Manual restore**: Use the restore functionality if needed
4. **Check permissions**: Ensure write access to VSCode directories

## 🤝 Contributing

Feel free to submit issues and enhancement requests!

## 📄 License

This project is provided as-is for educational and productivity purposes.

---

**🚀 Happy coding with Blackbox AI! 🚀**
