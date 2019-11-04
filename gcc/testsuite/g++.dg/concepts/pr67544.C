// { dg-do compile { target c++11 } }
// { dg-options "-fconcepts" }

template <typename T> struct A
{
  struct B;
};

struct C
{
  template<typename T> friend struct A<T>::B;
};
