// PR c++/57891
// { dg-do compile { target c++11 } }

template<int N, unsigned char M = N> struct S { char a[N]; };
S<1000> s; // { dg-error "narrowing conversion" }
