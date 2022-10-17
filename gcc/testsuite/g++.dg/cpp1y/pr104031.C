// PR c++/104031
// { dg-do run { target c++14 } }
// { dg-options "-O2" }

struct A {
  A () {}
  ~A () {}
};
struct B {
  A a;
  int b = 0;
};
struct C
{
  [[gnu::noipa]]
  C (B x) { if (x.b != 42) __builtin_abort (); }
};
static C c ({ .a = A{}, .b = 42 });

int
main ()
{
}
