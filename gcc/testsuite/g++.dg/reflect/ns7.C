// PR c++/127349
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

template<class> struct Y {};
namespace M { template<class> struct W {}; }
namespace N {
  template<class> struct X {};
  template<class T> using A = X<T>;
  inline namespace I { template<class> struct V {}; }
  using namespace M;
  template<class, class> struct Z {};
  int v;
  template<class T> void fn ();
}

template<template<class> class> struct S {};

template<auto ns>
void
f ()
{
  typename [:ns:]::template X<int> t1;
  typename [:ns:]::template A<int> t2;
  typename [:ns:]::template V<int> t3;
  typename [:ns:]::template W<int> t4;
  typename [:ns:]::template Y<int> t5; // { dg-error "'Y' is not a member of 'N'" }
  typename [:ns:]::template v<int> t6; // { dg-error "'N::v' is not a template" }
  typename [:ns:]::v t7;               // { dg-error "'N::v' is not a type" }
  typename [:ns:]::X t8;               // { dg-error "is not a type" }
  typename [:ns:]::template fn<int> t9;	// { dg-error "not a template" }
}

void
h ()
{
  f<^^N>();
}
