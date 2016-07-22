// PR c++/71739
// { dg-do compile { target c++11 } }

template <int N> struct alignas(N) A;
template <int N> struct alignas(N) A {};
