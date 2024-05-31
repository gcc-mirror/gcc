// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
  concept C1 = __is_same_as(T, int);

template<int N>
  concept C2 = N == 0;

template<template<typename> class X>
  concept C3 = true;

template<typename> struct Foo;

template<C1...> struct S1;
template<C1... Ts> struct S1 { };

template<C2...> struct S2;    // { dg-error "does not constrain a type" }
template<C2... Ns> struct S2 { }; // { dg-error "does not constrain a type" }

template<C3...> struct S3;    // { dg-error "does not constrain a type" }
template<C3... Xs> struct S3 { }; // { dg-error "does not constrain a type" }

S1<int, int, int> s1; // OK
S1<> s11;
S2<0, 0, 0> s2;
S2<> s22;   // { dg-error "" }
S3<Foo, Foo> s3;  // { dg-error "" }
S3<> s33; // { dg-error "" }
