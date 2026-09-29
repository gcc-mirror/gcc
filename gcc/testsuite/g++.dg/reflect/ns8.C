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
  S<[:ns:]::template X> s1;
  S<[:ns:]::template A> s2;
  S<[:ns:]::template V> s3;
  S<[:ns:]::template W> s4;
  S<[:ns:]::template Y> s5; // { dg-error "'Y' is not a member of 'N'" }
  S<[:ns:]::template v> s6; // { dg-error "'N::v' is not a class template" }
  S<[:ns:]::template fn> s7; // { dg-error "not a class template" }
  S<[:ns:]::template Z> s8; // { dg-error "mismatch" }
}
void h () { f<^^N>(); }

template<auto ns>
void h2 () {
  [] <class T> () { S<[:ns:]::template X> s; }.template operator()<int> ();
}
void f2 () { h2<^^N>(); }

template<auto ns>
void h3 () {
  [] <auto r> () { S<[:r:]::template X> s; }.template operator()<ns> ();
}
void f3 () { h3<^^N>(); }
