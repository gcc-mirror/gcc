// PR c++/91264
// { dg-do compile { target c++14 } }

struct D { int n; };

struct C { const D d; }; // { dg-message "originally declared" }

struct A {
  C c;
  constexpr A() : c{} { }
};

struct B {
  A a;
  constexpr B() {
    int &r = const_cast<int &>(a.c.d.n);
    r = 3; // { dg-error "modifying a const object" }
  }
};

constexpr B b{}; // { dg-message "in .constexpr. expansion of " }
static_assert((b.a.c.d.n, 1), "");
