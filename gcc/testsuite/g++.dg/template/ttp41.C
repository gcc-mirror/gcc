// PR c++/108347

template<class T>
struct A {
  template<class U> struct C { };

  template<template<class> class TT, class U>
  struct B;

  template<class U>
  struct B<C, U*>;

  template<class U>
  struct B<C, const U*> { };
};

A<int>::B<A<int>::C, const int*> b;
