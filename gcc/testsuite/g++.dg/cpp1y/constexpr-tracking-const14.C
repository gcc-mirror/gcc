// PR c++/91264
// { dg-do compile { target c++14 } }

struct F {
  const int f;
  constexpr F() : f(9) { }
};

struct C {
  int n;
  const F f;
  constexpr C() : n(1) { n = 66; }
};

struct A {
  int r;
  const C c; // { dg-message "originally declared" }
  constexpr A() : r(11) { r = 14; const_cast<C &>(c).n = 42; } // { dg-error "modifying a const object" }
};

struct D {
  const A a;
  constexpr D() { } // { dg-message "in .constexpr. expansion of" }
};

struct E {
  const D d;
  constexpr E() { } // { dg-message "in .constexpr. expansion of" }
};

struct B {
  const E e;
  constexpr B(bool) { } // { dg-message "in .constexpr. expansion of" }
};

constexpr B b(false); // { dg-message "in .constexpr. expansion of" }
static_assert(b.e.d.a.c.n == 2, ""); // { dg-error "non-constant condition" }
