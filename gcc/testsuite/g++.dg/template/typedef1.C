// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 9 Dec 2001 <nathan@nathan@codesourcery.com>

// PR 72

template <typename T> struct A
{
  typedef T type;
};

template <typename T> struct B
{
  typedef int xxx;
  typedef T xxx;
  typedef typename A<T>::type xxx;
  typedef A<int>::type xxx;
};

B<int> good;
