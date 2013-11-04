// { dg-options -std=c++11 }

struct A {
  int i;
  constexpr A():i(42) { };
};
struct B: A { };
constexpr int f(B b) { return b.i; }

struct C { C(); };	       // { dg-message "calls non-constexpr" }
struct D: C { };	       // { dg-message "no constexpr constructor" }
constexpr int g(D d) { return 42; } // { dg-error "invalid type" }
