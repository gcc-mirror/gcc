// PR c++/27933
// { dg-do compile }

template<int> struct A
{
  int i;
  A() { using i; }  // { dg-error "nested-name-specifier|declared" }
};

A<0> a;
