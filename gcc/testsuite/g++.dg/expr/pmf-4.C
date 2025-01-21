// PR c++/118509
// { dg-do run }
// { dg-options "-Wall -O2" }

struct A { void foo () { a = 1; } int a; A () : a (0) {} };
struct B : virtual A {};
typedef void (A::*C) ();

__attribute__((noipa)) void
foo (C x, B *y)
{
  (y->*x) ();
}

int
main ()
{
  B b;
  foo (&A::foo, &b);
  if (b.a != 1)
    __builtin_abort ();
}
