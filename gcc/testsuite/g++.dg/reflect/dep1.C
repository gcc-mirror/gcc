// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test dependent splice specifiers.

template<template<class> class X>
struct S {
  typename [: ^^X :]<int, float> m; // { dg-error "wrong number of template arguments" }
};

template<class> struct V1 {};
template<class, class = int> struct V2 {};

S<V1> s1; // { dg-message "required from here" }
S<V2> s2; // OK
