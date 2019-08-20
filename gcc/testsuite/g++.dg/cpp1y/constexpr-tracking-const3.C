// PR c++/91264
// { dg-do compile { target c++14 } }

struct A {
  int n;
  constexpr A() : n(1) { n = 2; }
};

struct B {
  const A a;
  constexpr B(bool b) {
    if (b)
      const_cast<A &>(a).n = 3; // { dg-error "modifying a const object" }
    }
};

constexpr B b(false);
static_assert(b.a.n == 2, "");

constexpr B b2(true); // { dg-message "in .constexpr. expansion of " }
// { dg-message "originally declared" "" { target *-*-* } .-1 } 
static_assert((b2.a.n, 1), "");
