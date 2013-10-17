// PR c++/58596
// { dg-do compile { target c++11 } }

struct A
{
  int i = [] { return decltype(i)(); }();
};
