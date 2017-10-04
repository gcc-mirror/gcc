// { dg-options -std=c++17 }

template <class T>
struct A
{
  int i;
};

struct B : A<int> {} b;

A a(b);
