// PR c++/70413
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler-not "(weak|glob)\[^\n\]*_Z" } }

namespace {
  template<class> struct A;
}

template<template<class> class Q> void f() { }

int main() {
  f<A>();
}
