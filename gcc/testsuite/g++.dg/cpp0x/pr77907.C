// PR c++/77907
// { dg-do run { target c++11 } }
// { dg-options "-O2 -save-temps" }
// { dg-final { scan-assembler-not "static_initialization" } }

struct A {
  int foo () { return 1; }
};

struct B {
  using C = int (A::*) ();
  constexpr explicit B (const C x) : b{x} {}
  C b;
};

B b{&A::foo};

int
main ()
{
  if (!b.b)
    __builtin_abort ();
}
