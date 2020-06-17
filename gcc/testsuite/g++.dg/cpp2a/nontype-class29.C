// PR c++/92948 - Fix class NTTP with template arguments.
// { dg-do run { target c++20 } }

struct A {
  int i;
  constexpr A(int n) : i(n) { }
};

template<A a>
struct B {
  static constexpr int i = a.i;
};

template<int X>
void foo()
{
  B<X> b;
  if (b.i != 42)
    __builtin_abort ();
}

int
main ()
{
  foo<42>();
}
