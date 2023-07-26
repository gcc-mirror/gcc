// PR c++/110566
// { dg-do compile { target c++20 } }

template<template<int N, int M> class>
struct A;

template<class T>
struct B {
  template<auto X, auto Y> struct C;
};

using type = A<B<int>::C>;
