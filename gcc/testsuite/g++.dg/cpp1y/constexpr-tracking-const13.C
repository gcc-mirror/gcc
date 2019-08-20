// PR c++/91264
// { dg-do compile { target c++14 } }

struct A {
  mutable int i;
  constexpr A() : i(0) { }
};
struct B {
  A a;
  constexpr B() : a{} { }
};

constexpr void
g ()
{
  const B b;
  b.a.i = 42;
}

static_assert((g(), 1), "");
