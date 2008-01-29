// { dg-options "-std=c++0x" }
template<typename> struct A {};

template<template<typename> class... T> void foo(T<int>) {} // { dg-error "not expanded|T" }

template void foo<A>(A<int>); // { dg-error "does not match" }
