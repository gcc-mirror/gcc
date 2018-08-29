// PR c++/71662
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-return-type" }

template < typename T > struct A
{
  enum E : T;
  E h ();
};

A < int > a;
A < int >::E b = a.h ();

template < typename T > enum A < T >::E : T { e };  // { dg-message "enumeration" }

template < typename T > typename A < T >::E A < T >::h ()
{
  return e;  // { dg-error "declared" }
}
