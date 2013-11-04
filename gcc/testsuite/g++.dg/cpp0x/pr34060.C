// PR c++/34060
// { dg-do compile }
// { dg-options "-std=c++11" }

template <int> struct A
{
  template <typename... > struct B {};
  template <typename... T> struct B <int, T *> {}; // { dg-error "not expanded|T" }
};

A<0>::B<int>b;
