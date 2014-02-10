// PR c++/59989
// { dg-require-effective-target c++11 }

template<typename T> struct X {};
template<template<typename...> class D, typename ...U> int test(D<U...>*);
int n = test<X, int>(0);	// { dg-error "no match" }
