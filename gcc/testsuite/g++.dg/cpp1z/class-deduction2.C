// { dg-do compile { target c++17 } }

template <class T>
struct A
{
  template <class U>
  A(T, U);
};

A a (42, 1.0);
