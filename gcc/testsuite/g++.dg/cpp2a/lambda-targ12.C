// PR c++/119401
// { dg-do compile { target c++20 } }

template <class>
struct B {};
template <int N>
struct A {
  void f(B<decltype([]{})>) {}
};
auto t = &A<0>::f;

// A<0>::f(B<A<0>::{lambda()#1}>)
// { dg-final { scan-assembler "_ZN1AILi0EE1fE1BINS0_UlvE_EE" { xfail *-*-* } } }
