// P0634R3
// { dg-do compile { target c++20 } }

template<typename T>
struct A
{
  typedef int I;
};

template<typename T> struct B
{
  A<T>::I i;
  typename A<T>::I i2;

  A<int>::I i3;
  typename A<int>::I i4;
};
