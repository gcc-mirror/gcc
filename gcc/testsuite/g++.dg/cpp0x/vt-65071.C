// PR c++/65071
// { dg-do compile { target c++11 } }

template<int> struct S {};

template<template<int> class... T, int N>
S<sizeof...(T)> foo(T<N>...);

auto x = foo(S<2>{});
