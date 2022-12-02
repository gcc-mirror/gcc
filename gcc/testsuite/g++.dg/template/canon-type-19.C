// PR c++/107539
// { dg-do compile { target c++11 } }

template<class T>
struct A { };

template<template<class> class C>
struct B {
  template<class T>
  void f(T t) {
    A<C<decltype(t)>> a1;
  }

  template<class T>
  void g(T t) {
    A<C<decltype(t)>> a2;
  }
};
