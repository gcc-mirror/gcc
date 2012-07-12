// PR c++/22233
// Origin: Volker Reichelt  <reichelt@igpm.rwth-aachen.de>
// { dg-do compile }

template<int> struct A
{
  A();
};

template<int N, char> A<N>::A() {}  // { dg-error "got 2 template parameters|1 required" }

A<0> a;
