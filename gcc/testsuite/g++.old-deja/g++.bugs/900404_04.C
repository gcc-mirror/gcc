// g++ 1.37.1 bug 900404_04

// [dcl.dcl] explains that simple-declarations may omit the
// init-declarator-list only if the decl-specifier-seq declares a
// class, i.e. if it contains a class-specifier, an
// elaborated-type-specifier with class key, or an enum-specifier. The
// declaration below contains neither.

// g++ fails to flag errors for such usage.

// keywords: semicolon, vacuous, file scope, declaration

int i;

;			// ERROR - , XFAIL *-*-*

int main () { return 0; }
