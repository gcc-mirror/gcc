// { dg-do compile { target c++11 } }
// { dg-additional-options -fabi-compat-version=0 }

namespace N {
  template <class T> using A = typename T::template X<int>;
}

template<template<class> class TT> struct B { };

// { dg-final { scan-assembler "_Z1f1BIN1N1AEE" } }
void f(B<N::A>) {}
