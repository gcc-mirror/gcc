// PR c++/117259
// { dg-do compile }
// { dg-options "-Wuninitialized -fsanitize=undefined" }

struct A { void foo () {} };
struct B { void (A::*b) (); B (void (A::*x) ()) : b(x) {}; };
const B c[1] = { &A::foo };

void
foo (A *x, int y)
{
  (x->*c[y].b) ();
}
