// PR c++/127344
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

template<class, class> struct same_type;
template<class T> struct same_type<T, T> {};

namespace X { struct N { static constexpr int x = 42; }; }

template<auto ns>
int
f ()
{
  namespace A = [:ns:];
  int i = A::N::x;
  int j = [:ns:]::N::x;
  return i + j;
}

namespace N { template<class> struct X {}; }
template<template<class> class> struct S {};

template<auto ns>
void
h ()
{
  namespace A = [:ns:];
  S<A::template X> s;
}

namespace Y { int y = 1; void fn (); namespace N { int x = 2; } }

template<auto ns>
int
k ()
{
  namespace A = [:ns:];
  namespace B = A;
  auto p = &A::fn;
  same_type<decltype(p), void (*) ()>();
  auto q = &[:ns:]::fn;
  same_type<decltype(q), void (*) ()>();
  return A::y + B::N::x;
}

void
g ()
{
  f<^^X>();
  h<^^N>();
  k<^^Y>();
}
