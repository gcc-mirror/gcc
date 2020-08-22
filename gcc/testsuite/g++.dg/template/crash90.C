// PR c++/39750

template < unsigned >
struct A ;
template < typename >
struct B ;
template < typename T , A < B < T > {} // { dg-error "parse error|non-type|initializer" }

// { dg-error "-:expected" "" { target *-*-* } .+1 }
