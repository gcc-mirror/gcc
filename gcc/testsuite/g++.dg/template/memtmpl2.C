// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 18 Sep 2003 <nathan@codesourcery.com>

// PR c++/12332. ICE

template <unsigned D> class TPL;

template <typename T> struct X {
  template <template <typename> class V>
  V<TPL<V<int>::d> > operator () ();
};

void Foo (X<int> x) {}
