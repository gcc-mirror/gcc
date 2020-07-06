// { dg-do assemble  }
// g++ 1.37.1 bug 900404_04

// [dcl.dcl] explains that simple-declarations may omit the
// init-declarator-list only if the decl-specifier-seq declares a
// class, i.e. if it contains a class-specifier, an
// elaborated-type-specifier with class key, or an enum-specifier. The
// declaration below contains neither.

// Since C++11 this is allowed as an empty-declaration.

// g++ fails to flag errors for such usage in C++98.

// keywords: semicolon, vacuous, file scope, declaration

int i;

;			// { dg-error "extra ';'" "" { target c++98_only } 0 } 

int main () { return 0; }
