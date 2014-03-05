// PR c++/39750

template < unsigned >
struct A ;
template < typename >
struct B ;
template < typename T , A < B < T > {}// { dg-error "initializer|parse error|type|expected" }
