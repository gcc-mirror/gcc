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
  typedef int xxx; // { dg-error "" }
  typedef T xxx; // { dg-error "" }
  typedef typename A<T>::type xxx; // { dg-error "" }
  typedef A<int>::type xxx; // { dg-error "" }
};

B<int> good;
