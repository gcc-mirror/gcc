// { dg-options "-std=c++0x" }
template<int...> struct A;

template<template<int> class... T> struct A<T...> {}; // { dg-error "mismatch|expected" }
