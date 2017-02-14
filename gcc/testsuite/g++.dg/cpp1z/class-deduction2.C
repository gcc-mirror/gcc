// { dg-options -std=c++1z }

template <class T>
struct A
{
  template <class U>
  A(T, U);
};

A a (42, 1.0);
