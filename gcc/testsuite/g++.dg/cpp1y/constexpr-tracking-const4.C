// PR c++/91264
// { dg-do compile { target c++14 } }

struct A {
  const int n; // { dg-message "originally declared" }
  constexpr A() : n(1) { }
};
struct B {
  A a;
  constexpr B() {
    int *p = const_cast<int *>(&a.n);
    *p = 3; // { dg-error "modifying a const object" }
  }
};
constexpr B b; // { dg-message "in .constexpr. expansion of " }
static_assert((b.a.n, 1), "");
