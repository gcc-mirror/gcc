// PR c++/27496
// { dg-do compile }

template<int> struct A
{
  template<void> friend class X;  // { dg-error "void|valid type" }
};

A<0> a;
