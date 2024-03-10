// PR c++/109651
// { dg-do compile { target c++20 } }

template<class T>
void f() {
  []<class U>(U) {
    []<template<class> class TT>(TT<int>) { };
  }(0);
}

template void f<int>();
