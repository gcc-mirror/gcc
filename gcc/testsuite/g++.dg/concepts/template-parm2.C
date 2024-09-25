// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
  concept C1 = __is_same_as(T, int);

template<int N>
  concept C2 = N == 0;

template<template<typename> class X>
  concept C3 = false;

template<typename> struct Foo;

// Instantiation of default arguments happens at the point of
// instantiation for the class.

template<C1 T = char> struct S1 { };
template<C2 N = 1> struct S2 { };   // { dg-error "does not constrain a type" }
template<C3 X = Foo> struct S3 { }; // { dg-error "does not constrain a type|missing" }

S1<> s1; // { dg-error "constraint failure|invalid type" }
S2<> s2; // { dg-error "constraint failure|invalid" }
S3<> s3; // { dg-error "constraint failure|invalid" }
