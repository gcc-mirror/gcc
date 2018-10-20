// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  concept bool D() { return C<T>() and __is_empty(T); }

template<template<typename Q> requires C<Q>() class X>
  struct S { };

template<typename A> requires true struct T0 { };
template<typename A> requires D<A>() struct T1 { };

S<T0> x3; // { dg-error "constraint mismatch|invalid type" }
S<T1> x4; // { dg-error "constraint mismatch|invalid type" }

int main() { }
