// PR c++/59347

template<int> struct A
{
  typedef int ::X;		// { dg-error "15:typedef name" }
};

A<0> a;
