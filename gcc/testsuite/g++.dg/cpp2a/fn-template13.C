// P0846R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

struct A {
  template<typename T>
  int foo (T a, T b) { return a + b; }
};

int
bar (A* pa, int (A::*pm)(int, int))
{
  return (pa->*pm)(1, 2);
}

int
baz (A pa, int (A::*pm)(int, int))
{
  return (pa.*pm)(1, 2);
}

int
main ()
{
  A a;
  int i = bar (&a, &A::foo<int>);
  if (i != 3)
    __builtin_abort ();
  i = baz (a, &A::foo<int>);
  if (i != 3)
    __builtin_abort ();
}
