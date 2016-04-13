// { dg-options "-std=c++1z -fconcepts" }

template<int N, class T>
  concept bool P() { return true; }

template<template<typename> class X, class T>
  concept bool Q() { return true; }

template<P<int> N> void f() { }
template<Q<int> X> void g() { }

template<typename> struct S { };

int main() {
  f<0>();
  g<S>();
}
