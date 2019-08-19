// PR c++/91264
// { dg-do compile { target c++14 } }

struct A {
  mutable int n;
  constexpr A() : n(1) { n = 2; }
};

struct B {
  const A a;
  constexpr B() {
    const_cast<A &>(a).n = 3;
  }
};

constexpr B b{};
static_assert((b.a.n, 1), "");
