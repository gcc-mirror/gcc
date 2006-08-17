// PR c++/28711
// { dg-do compile }
// { dg-options "" }

template<int> struct A
{
  int x[1][1];
  A() : x((int[1][]){{0}}) {}  // { dg-error "except the first" }
};

A<0> a;
