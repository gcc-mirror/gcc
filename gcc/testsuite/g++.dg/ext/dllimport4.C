//  Report error if dllimport attribute in definition itself.
// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw*} }

__attribute__((dllimport))  void bar () { }	// { dg-error "definition" }

__attribute__((dllimport))  int foo = 1;	// { dg-error "definition" }
