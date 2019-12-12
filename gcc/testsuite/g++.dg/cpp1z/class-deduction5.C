// { dg-do compile { target c++17 } }

template <class T>
struct A
{
  int i;
};

A<int> a1;
A a(a1);
