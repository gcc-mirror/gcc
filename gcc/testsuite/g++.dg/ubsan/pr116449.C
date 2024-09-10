// PR c++/116449
// { dg-do compile }
// { dg-options "-O2 -Wall -fsanitize=undefined" }

struct C { void foo (int); void bar (); int c[16]; };
typedef void (C::*P) ();
struct D { P d; };
static D e[1] = { { &C::bar } };

void
C::foo (int x)
{
  (this->*e[c[x]].d) ();
}
