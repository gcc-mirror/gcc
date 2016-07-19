// PR c++/71054
// { dg-do compile { target c++11 } }

#include <initializer_list>

template <typename D, typename T = decltype (&D::U)>
struct S
{
  struct A
  {
    int a;
    int b;
    T p;
  };
  S () { std::initializer_list<A> a{ {0, 0, &D::V} }; }
};
struct R {
  void V (int);
  void U (int);
};
S<R> b;
