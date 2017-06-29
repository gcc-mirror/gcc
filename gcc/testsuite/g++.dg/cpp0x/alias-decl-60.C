// PR c++/72764
// { dg-do compile { target c++11 } }

template < typename > struct A;
template < typename > struct B {};

template < typename T >
using C = typename A < T >::template D < T >;

template < typename T > struct A
{ 
  // should be: template < typename > struct D : B < C < T > > {};
  struct D : B < C < T > > {};	// { dg-error "not a class template" }
};

A < int >::D a;			// { dg-message "required" }
