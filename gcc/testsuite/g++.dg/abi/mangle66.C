// { dg-do compile { target c++11 } }

namespace N {
  template <class T> using A = typename T::template X<int>;
}

template<template<class> class TT> struct B { };

// { dg-final { scan-assembler "_Z1f1BIN1N1AEE" } }
void f(B<N::A>) {}
