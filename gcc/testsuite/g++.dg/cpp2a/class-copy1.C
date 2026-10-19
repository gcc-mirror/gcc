// PR c++/127710
// { dg-do run { target c++20 } }

struct S { int i; };
struct A {
  A() = default;
  A(int i, char c) : i(i), c(c) {}
  int i;
  char c;
};
struct B : A { };
// Try an empty closure as a base.
struct W : decltype([] {}) { int i; };

int
main ()
{
  S s = S{} = S{1};
  if (s.i != 1)
    __builtin_abort ();

  B b = B{} = B{{2, 'x'}};
  if (b.i != 2 || b.c != 'x')
    __builtin_abort ();

  W w = W{} = W{{}, 3};
  if (w.i != 3)
    __builtin_abort ();
}
