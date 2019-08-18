// PR c++/38794
// { dg-do compile }

typedef void foo () {}	// { dg-error "14:typedef may not be a function definition" }
struct S
{
  typedef int bar (void) { return 0; } // { dg-error "15:typedef may not be a member function definition" }
};
