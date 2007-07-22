// { dg-do compile }

// Copyright (C) 2007 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 21 Jul 2007 <nathan@codesourcery.com>

template <typename T> struct A
{
  struct B;
  typedef typename B::type type;
};

template <typename T> struct A<T>::B
{
  typedef typename A<T>::type type;

  type Foo ();
};

template <typename T>
typename A<T>::B::type
A<T>::B::Foo () 
{
  return 0;
}
