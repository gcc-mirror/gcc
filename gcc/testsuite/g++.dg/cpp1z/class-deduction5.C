// { dg-options -std=c++1z }

template <class T>
struct A
{
  int i;
};

A<int> a1;
A a(a1);
