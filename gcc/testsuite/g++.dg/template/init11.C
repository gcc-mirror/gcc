// PR c++/79607
// { dg-do compile { target c++11 } }

template<typename T> struct A
{
  static const int i = int{T{}};
};

A<int> a;
