// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
  concept bool C1 = __is_same_as(T, int);

template<int N>
  concept bool C2 = N == 0;

template<template<typename> class X>
  concept bool C3 = false;

template<typename> struct Foo;

template<C1... Ts> struct S1 { };
template<C2... Ns> struct S2 { };
template<C3... Xs> struct S3 { };

S1<int, int, bool> s1; // { dg-error "constraint failure|invalid type" }
S2<0, 1, 2> s2; // { dg-error "constraint failure|invalid type" }
S3<Foo> s3; // { dg-error "constraint failure|invalid type" }
