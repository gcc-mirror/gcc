// PR c++/107906
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler-not "(weak|glob)\[^\n\]*_Z" { xfail *-*-* } } }

namespace {
  template<class> using X = int;
  struct A {
    template<class> using X = int;
  };
}
template<template<class> class Q> void f() { }

int main() {
  f<X>();
  f<A::X>();
}
