// PR c++/32561

template<int N, int N> struct A; // { dg-error "redefinition|declared" } 
