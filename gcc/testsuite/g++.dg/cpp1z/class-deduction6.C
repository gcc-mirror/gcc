// { dg-options -std=c++1z }

template <class T>
struct A
{
  int i;
};

struct B : A<int> {} b;

A a(b);
