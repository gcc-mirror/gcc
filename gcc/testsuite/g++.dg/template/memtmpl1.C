// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 30 Jul 2003 <nathan@codesourcery.com>

// PR 11347. ICE in tsubst

template <class T> struct T1 {
  enum {N};
};

template<class T> struct T2 {
  template <class S, bool Z = T1<S>::N + 1> struct B {};
  struct C {};
};

T2<int> t;

T2<int>::B<int> s;

