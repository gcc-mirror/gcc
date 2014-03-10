// { dg-do compile { target c++11 } }
template<typename> struct A {};

template<template<typename> class... T> void foo(T<int>) {} // { dg-error "not expanded|T" }

template void foo<A>(A<int>); // { dg-error "does not match" }
