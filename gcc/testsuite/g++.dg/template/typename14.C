// { dg-do compile }

// Copyright (C) 2007 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 21 Jul 2007 <nathan@codesourcery.com>

template <typename T> struct A  
{
  typedef const T X;
  
  struct B;
};

template <typename T> struct A<T>::B
{
  typedef volatile typename A<T>::X Y;

  T const volatile *Foo ();
};

template<typename T>
typename A<T>::B::Y *A<T>::B::Foo ()
{
  return 0;
}
