// PR c++/22233
// Origin: Volker Reichelt  <reichelt@igpm.rwth-aachen.de>
// { dg-do compile }

template<int> struct A // { dg-error "declaration" }
{
  A();
};

template<int N, char> A<N>::A() {}  // { dg-error "invalid use of incomplete type" }

A<0> a;
