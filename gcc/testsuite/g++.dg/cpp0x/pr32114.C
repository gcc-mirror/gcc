// { dg-options "-std=c++11" }
template<typename ...T> struct A
{
  typedef typename T::X Y; // { dg-error "not expanded|T" }
};

A<int> a;
