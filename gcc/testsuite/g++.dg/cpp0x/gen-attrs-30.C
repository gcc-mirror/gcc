// { dg-do compile { target c++11 } }
// PR c++/35074

template<typename T> struct A
{
  void foo() const;
} [[gnu::aligned(4)]]; // { dg-warning "ignored" }

template<typename T> void A<T>::foo() const {}
