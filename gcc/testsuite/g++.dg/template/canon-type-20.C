// PR c++/109531
// { dg-do compile { target c++11 } }
// { dg-additional-options "--param=hash-table-verification-limit=1000" }

template<class T>
using A = int;

struct B { using type = int; };
struct C { using type = A<int>; };

template<class T>
struct D {
  template<template<class> class TT>
  TT<typename T::type> f();
};

template struct D<B>;
template struct D<C>;
