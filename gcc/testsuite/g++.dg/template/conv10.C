// PR c++/41994

template<typename T> struct A
{
  operator T();
  A() { T (A::*f)() = &A::operator T; }
};

A<int> a;
