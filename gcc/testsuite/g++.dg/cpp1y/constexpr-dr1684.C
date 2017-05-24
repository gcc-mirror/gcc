// DR 1684
// { dg-do compile { target c++11 } }

struct A {
  A(int);
  constexpr int foo() { return 0; } // { dg-error "literal" "" { target c++11_only } }
};
