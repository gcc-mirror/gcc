//PR c++/28738

template<int,void> struct A {};    // { dg-error "not a valid type" }
template<int N> struct A<N,0> {};
