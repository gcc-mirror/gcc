// PRMS Id: 4687
// Bug: g++ misinterprets typedefs of function type in class scope.
// Build don't link:

struct A {
   typedef int F();
   F *fp;
   void* g() { return fp; }	// gets bogus error - typing
};
