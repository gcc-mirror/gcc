// PR c++/101670
// { dg-do compile { target c++20 } }

template<int N, auto = []{}>
struct A;

template<int N>
struct A<N> { }; // Partial specialization unusable due to lambda uniqueness

A<0> a; // { dg-error "incomplete" }
