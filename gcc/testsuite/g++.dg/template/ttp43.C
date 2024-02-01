// PR c++/112737
// { dg-do compile { target c++11 } }

template<template<class> class, class T>
void g(T);

template<template<class> class TT, class T>
decltype(g<TT>(T{})) f(T); // #1

template<template<class> class TT, class T>
decltype(g<TT>(T{})) f(T); // redeclaration of #1

template<class T> struct A;

int main() {
  f<A>(0);
}
