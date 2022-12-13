// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template<typename T>
  concept bool C1 = __is_same_as(T, int);

template<int N>
  concept bool C2 = N == 0;

template<template<typename> class X>
  concept bool C3 = true;

template<typename> struct Foo;

template<C1...> struct S1;
template<C1... Ts> struct S1 { };

template<C2...> struct S2;
template<C2... Ns> struct S2 { };

template<C3...> struct S3;
template<C3... Xs> struct S3 { };

S1<int, int, int> s1; // OK
S1<> s11;
S2<0, 0, 0> s2;
S2<> s22;
S3<Foo, Foo> s3;
S3<> s33;
