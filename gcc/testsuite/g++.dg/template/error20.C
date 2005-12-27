// PR c++/25439

template<int> struct A;
template<> int A<0>; // { dg-error "invalid" }
