// PR c++/59347

template<int> struct A
{
  typedef int ::X;		// { dg-error "" }
};

A<0> a;
