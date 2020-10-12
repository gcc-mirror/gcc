// { dg-do compile { target c++20 } }
// { dg-additional-options -Wno-inaccessible-base }

struct Base { int i; };
template <int N> struct Derived : Derived<N-1>, Base {};
template <> struct Derived<0> : Base {};

template <int* P> struct A { };

Derived<4> d;
void f(A<&((Derived<0>&)d).i>) {}

// { dg-final { scan-assembler _Z1f1AIXaddtcvR7DerivedILi0EEL_Z1dE1iEE } }
