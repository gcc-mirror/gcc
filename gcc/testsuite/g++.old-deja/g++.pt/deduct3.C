// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 18 Jan 2001 <nathan@codesourcery.com>

// Bug 1694. We complained during deduction, rather than reject the deduction.

template <class T, T d> class X {};

template <class T> X<T,0> Foo (T *);
template <class T> int Foo (T const *);

void Baz (int *p1, int const *p2)
{
  int i = Foo (p1); // { dg-error "" } cannot convert
  int j = Foo (p2);
}
void Baz (float *p1, float const *p2)
{
  int i = Foo (p1); // ok, deduction fails on X<T,0> Foo (T *)
  int j = Foo (p2);
}
