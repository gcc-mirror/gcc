// PR c++/46472
// { dg-do compile { target c++11 } }

template<class T> struct A {
  T t;
  constexpr A(){}
};

struct B
{
  A<int> a;
};

B b;
