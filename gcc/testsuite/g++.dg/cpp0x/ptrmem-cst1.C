// PR c++/107574
// { dg-do compile { target c++11 } }

struct A { int i; };
struct B:A { int j; };
struct C:B {
  int k;
  static_assert((int B::*) &C::k, ""); // { dg-error "non-constant|still incomplete" }
};

static_assert((int B::*) &C::k, "");
