// { dg-options "-std=c++98 -pedantic-errors" }
// { dg-do assemble  }
// Origin: Jason Merrill <jason@cygnus.com>

template <class T> struct A
{
  struct B;
};

template<class T> struct C
{
  friend typename A<T>::B; // { dg-error "" } `typename' not allowed
};

