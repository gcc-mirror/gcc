// PR tree-optimization/104334
// { dg-do run { target c++11 } }
// { dg-options "-O2 --param logical-op-non-short-circuit=0" }

enum class A { A0, A1, A2, A3 };
int x;

__attribute__((noipa)) void
baz ()
{
  x = 1;
}

struct B {
  unsigned b : 2;

  A
  foo () const
  {
    return static_cast<A> (b);
  }

  __attribute__((noinline)) void
  bar ()
  {
    if (foo () == A::A2 || foo () == A::A3)
      baz ();
  }
};

int
main ()
{
  B c;
  c.b = 2;
  c.bar ();
  if (x != 1)
    __builtin_abort ();
  return 0;
}
