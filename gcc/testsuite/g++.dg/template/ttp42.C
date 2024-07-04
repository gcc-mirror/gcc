// PR c++/112737
// { dg-do compile { target c++17 } }

template<template<class> class TT>
decltype(TT{42}) f(); // #1

template<template<class> class TT>
decltype(TT{42}) f(); // redeclaration of #1

template<class T> struct A { A(T); };

int main() {
  f<A>();
}
