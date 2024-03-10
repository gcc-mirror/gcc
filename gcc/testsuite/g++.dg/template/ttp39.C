// PR c++/110566
// { dg-do compile { target c++20 } }

template<template<int N, int M> class>
struct A;

template<class U>
struct B {
  template<class T>
  struct C {
    template<template<auto X, auto Y> class TT>
    using type = A<TT>;
  };
};

template struct B<int>::C<int>;
