// PR c++/85285

template<typename T> union A
{
  T x;				// { dg-error "flexible array" }
};

A<int[]> a;
