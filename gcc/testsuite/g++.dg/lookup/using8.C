// { dg-do compile }
// Origin: Bill Clarke <llib at computer dot org>
// PR c++/11097: using declartion for a converter operator to a nested class
//  in a base type

template <typename T>
struct A
{
  struct Nested {};
  operator Nested*();
};

template <typename T>
struct B : A<T>
{
  using A<T>::operator typename A<T>::Nested*;
};

template struct B<int>;
