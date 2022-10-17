// PR c++/99909
// { dg-do compile { target c++20 } }

template<class T> constexpr bool always_true = true;
template<class T> concept C = always_true<T>;

template<C auto> struct S;

template<template<auto> class TT> void f() { }

template void f<S>();
