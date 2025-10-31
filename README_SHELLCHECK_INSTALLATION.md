# Shellcheck Installation Guide for Windows

This guide provides multiple methods to install shellcheck on Windows 10/11 systems.

## Quick Start

### Method 1: Automated Installation (Recommended)
Run the PowerShell installation script:
```powershell
powershell -ExecutionPolicy Bypass -File install_shellcheck.ps1
```

### Method 2: Manual Verification
Run the batch verification script:
```cmd
verify_shellcheck.bat
```

### Method 3: Direct winget Installation
```cmd
winget install --id koalaman.shellcheck
```

## Verification

After installation, verify shellcheck is working:
```cmd
test_shellcheck_functionality.bat
```

Or manually check:
```cmd
shellcheck --version
```

## Usage Examples

### Basic Usage
```bash
shellcheck script.sh
```

### Test with Provided Examples
```bash
# Test problematic script (will show issues)
shellcheck test_script_with_issues.sh

# Test improved script (should pass cleanly)
shellcheck test_script_improved.sh
```

### Different Output Formats
```bash
# JSON output
shellcheck -f json script.sh

# GCC-style output
shellcheck -f gcc script.sh

# Colored output
shellcheck --color=always script.sh
```

## Files Included

| File | Purpose |
|------|---------|
| `install_shellcheck.ps1` | Comprehensive PowerShell installation script |
| `verify_shellcheck.bat` | Batch script to verify installation |
| `test_shellcheck_functionality.bat` | Complete functionality test |
| `shellcheck_usage_example.md` | Detailed usage guide |
| `test_script_with_issues.sh` | Example script with common problems |
| `test_script_improved.sh` | Corrected version of the test script |
| `shellcheck_installation_todo.md` | Installation progress tracker |

## Alternative Installation Methods

### Chocolatey
```cmd
choco install shellcheck
```

### Scoop
```cmd
scoop install shellcheck
```

### Manual Download
1. Visit: https://github.com/koalaman/shellcheck/releases
2. Download the Windows binary (shellcheck-vX.X.X.windows.x86_64.zip)
3. Extract shellcheck.exe to a directory in your PATH
4. Verify with `shellcheck --version`

## Troubleshooting

### Shellcheck Not Found
- Ensure shellcheck.exe is in your PATH
- Try restarting your terminal/command prompt
- Run the installation scripts as Administrator if needed

### Permission Issues
- Run PowerShell as Administrator
- Use `-ExecutionPolicy Bypass` for PowerShell scripts

### Package Manager Not Available
- Install winget (should be pre-installed on Windows 10/11)
- Install Chocolatey: https://chocolatey.org/install
- Install Scoop: https://scoop.sh/

## Integration

### VS Code
Install the "shellcheck" extension for real-time linting in VS Code.

### Git Hooks
Add shellcheck to your pre-commit hooks to automatically check shell scripts.

### CI/CD
Integrate shellcheck into your continuous integration pipeline.

## Support

For issues with shellcheck itself, visit: https://github.com/koalaman/shellcheck
For Windows-specific installation issues, refer to the troubleshooting section above.
