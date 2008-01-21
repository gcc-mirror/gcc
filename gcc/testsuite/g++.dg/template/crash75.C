// PR c++/34776

template<typename T> struct A
{
  T::X<0> x; // { dg-error "non-template|T::template|base type" }
};

A<int*> a;
