// PR c++/121795
// A version of pack-indexing17.C using a nontype pack.
// { dg-do compile { target c++26 } }

template<int N, int... Ns>
struct A;

template<int... Ns>
struct A<Ns...[sizeof...(Ns)-1], Ns...> { };

A<0, 0> x;
A<0, 1> y; // { dg-error "incomplete" }
