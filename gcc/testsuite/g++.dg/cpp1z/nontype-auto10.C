// PR c++/80096
// { dg-do compile { target c++17 } }

template<auto> struct A
{
  template<int> struct B {};
};

A<0> a;
