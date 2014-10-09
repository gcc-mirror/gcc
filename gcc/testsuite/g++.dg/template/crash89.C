// PR c++/34397

template<typename T, int = T()[0]> struct A // { dg-error "subscripted" }
{
  typedef A<T> B;
};

A<int> a; // { dg-error "template argument 2 is invalid" }
