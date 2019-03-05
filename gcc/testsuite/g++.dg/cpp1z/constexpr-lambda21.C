// PR c++/85228
// { dg-do compile { target c++17 } }

template<int> struct A
{
  enum E { e = []{ return 0; }() };
};

template class A<0>;
