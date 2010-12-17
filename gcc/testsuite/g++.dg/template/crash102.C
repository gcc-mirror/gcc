// PR c++/45043

template < typename > class A;
template < typename T > A < T >::B::~B () // { dg-error "type" }
{}
