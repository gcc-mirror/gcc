// { dg-additional-options "-fconcepts" }

template <typename T> struct A
{
  struct B;
};

struct C
{
  template<typename T> friend struct A<T>::B;
};
