// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 Mar 2003 <nathan@codesourcery.com>

// PR 10224. Rejected a valid constant argument.

template <bool B> struct X {
  struct I {};
};

template <typename T> struct Y {
  static const bool selector = true;
  typedef typename X<selector>::I helper;
};

Y<int> i;
