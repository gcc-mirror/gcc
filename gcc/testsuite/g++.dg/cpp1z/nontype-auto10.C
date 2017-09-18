// PR c++/80096
// { dg-options -std=c++17 }

template<auto> struct A
{
  template<int> struct B {};
};

A<0> a;
