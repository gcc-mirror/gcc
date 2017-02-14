// { dg-options -std=c++1z }

template <class T>
struct A
{
  A(T);
};

A a (42);
