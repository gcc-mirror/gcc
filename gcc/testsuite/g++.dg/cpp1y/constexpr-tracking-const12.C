// PR c++/91264
// { dg-do compile { target c++14 } }

struct A {
  const int n;
  int m;
  constexpr A() : n(1), m(2) { }
};
struct B {
  A a;
  constexpr B() {
    int *p = &a.m;
    *p = 3;
  }
};
constexpr B b;
static_assert(b.a.m == 3, "");
