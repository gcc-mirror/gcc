// { dg-do assemble  }
// PRMS Id: 4687
// Bug: g++ misinterprets typedefs of function type in class scope.

typedef int (*F1) ();
struct A {
   typedef int F();
   F *fp;
   F1 g() { return fp; }	// { dg-bogus "" } typing
};
