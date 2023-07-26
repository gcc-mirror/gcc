// { dg-do compile { target c++17 } }

template<template<class> class>
struct A { };

template<class T>
struct B {
  template<template<class> class TT, A<TT>* = nullptr>
  B(TT<T>);
};

template<class T>
struct C { };

using type = decltype(B{C<int>{}});
using type = B<int>;
