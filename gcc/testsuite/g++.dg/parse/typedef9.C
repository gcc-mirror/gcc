// PR c++/38794
// { dg-do compile }

typedef void foo () {}	// { dg-error "invalid function declaration" }
struct S
{
  typedef int bar (void) { return 0; } // { dg-error "invalid member function declaration" }
};
