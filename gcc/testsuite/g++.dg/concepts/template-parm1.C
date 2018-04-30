// { dg-options "-std=c++17 -fconcepts" }

template<typename T>
  concept bool C1 = __is_same_as(T, int);

template<int N>
  concept bool C2 = N == 0;

template<template<typename> class X>
  concept bool C3 = true;

template<typename> struct Foo;

// Type template parameters
template<C1 T = int> struct S1 { };
template<C1 = int> struct S2;
template<C1 T> struct S2 { };

// Non-type template parameters
template<C2 N = 0> struct S3 { };
template<C2 = 0> struct S4;
template<C2 N> struct S4 { };

// Template template parameters
template<C3 X = Foo> struct S5 { };
template<C3 = Foo> struct S6;
template<C3 X> struct S6 { };

S1<> s1;
S2<> s2;
S3<> s3;
S4<> s4;
S5<> s5;
S6<> s6;
