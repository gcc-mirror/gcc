// { dg-do compile { target c++11 } }

struct A {
  int i;
  constexpr A():i(42) { };
};
struct B: A { };
constexpr int f(B b) { return b.i; }

struct C { C(); };	       // { dg-message "" "" { target c++20_down } }
struct D: C { };	       // { dg-message "" "" { target c++20_down } }
constexpr int g(D d) { return 42; } // { dg-error "invalid type" "" { target c++20_down } }
