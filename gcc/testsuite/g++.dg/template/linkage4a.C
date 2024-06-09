// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler "(weak|glob)\[^\n\]*_Z1fI1XEvv" } }
// { dg-final { scan-assembler "(weak|glob)\[^\n\]*_Z1fIN1A1XEEvv" } }

template<class> using X = int;
struct A {
  template<class> using X = int;
};
template<template<class> class Q> void f() { }

int main() {
  f<X>();
  f<A::X>();
}
