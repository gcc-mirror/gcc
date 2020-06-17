// { dg-do compile { target c++20 } }

template<typename T>
  concept C = __is_class(T);

template<typename T>
  concept D = C<T> and __is_empty(T);

template<template<typename Q> requires C<Q> class X>
  struct S { };

template<typename A> requires true struct T0 { };
template<typename A> requires D<A> struct T1 { };

S<T0> x3; // { dg-error "constraint mismatch|invalid type" }
S<T1> x4; // { dg-error "constraint mismatch|invalid type" }

int main() { }
