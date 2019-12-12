// { dg-do compile { target c++17 } }

template <class T>
struct A
{
  A(T);
};

A a (42);
