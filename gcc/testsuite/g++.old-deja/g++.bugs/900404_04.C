// g++ 1.37.1 bug 900404_04

// The ANSI C does not allow vacuous statements (i.e. just semicolons)
// at the file-scope level.

// The current C++ Reference Manual does not indicate whether these should
// be considered legal or not.

// I am forced to conclude that C++ will follow ANSI C in this regard,
// and that these are therefore not legal.

// g++ fails to flag errors for such usage.

// keywords: semicolon, vacuous, file scope, declaration

int i;

;			// ERROR - , XFAIL *-*-*

int main () { return 0; }
