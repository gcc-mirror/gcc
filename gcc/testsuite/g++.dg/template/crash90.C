// PR c++/39750

template < unsigned >
struct A ;
template < typename >
struct B ;
template < typename T , A < B < T > {} // { dg-error "parse error|non-type" }
// { dg-error "39:expected" "" { target *-*-* } .-1 }
// { dg-error "37:initializer list" "" { target c++98_only } .-2 }
