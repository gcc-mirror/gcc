// PR c++/46472
// { dg-options -std=c++0x }

template<class T> struct A {
  T t;
  constexpr A(){}
};

struct B
{
  A<int> a;
};

B b;
