// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 Dec 2002 <nathan@codesourcery.com>

// PR 3784: We were confusing non-type template parms.

template <unsigned N> class X { };

template <short N>       void foo1(X<N>);
template <unsigned N>  void foo2(X<N>);

int main() {
  X<2> x;
  foo2(x);
}
