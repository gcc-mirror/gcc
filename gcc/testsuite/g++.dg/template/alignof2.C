// PR c++/55337
// { dg-do compile }

template <int> struct A;
template <typename T> struct B
{
  static A <__alignof__ (T)> b;
};
template <typename T> A<__alignof__ (T)> B<T>::b;
