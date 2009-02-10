// PR c++/34397

template<typename T, int = T()[0]> struct A
{
  typedef A<T> B;
};

A<int> a; // { dg-error "subscripted|template|declaration" }
