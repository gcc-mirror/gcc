// { dg-do compile }
// PR c++/35074
template<typename T> struct A
{
  void foo() const;
} __attribute((aligned(4)));

template<typename T> void A<T>::foo() const {}
