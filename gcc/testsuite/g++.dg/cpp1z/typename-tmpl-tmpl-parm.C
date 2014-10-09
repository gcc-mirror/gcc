// { dg-do compile }
// { dg-options "" }

template<typename T>
  struct A {};

#if __cplusplus >= 201103L
template<typename T>
  using B = int;
#endif

template<template<typename> class X>
  struct C {};

C<A> ca;

#if __cplusplus >= 201103L
C<B> cb;
#endif

template<template<typename> typename X>
  struct D {};

D<A> da;

#if __cplusplus >= 201103L
D<B> db;
#endif
