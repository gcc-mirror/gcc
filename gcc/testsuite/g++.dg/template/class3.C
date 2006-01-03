// PR c++/25634
template<int> template<int> struct A; // { dg-error "too many" }
