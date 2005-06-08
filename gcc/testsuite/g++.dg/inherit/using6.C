// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 8 Jun 2005 <nathan@codesourcery.com>

struct A
{
  operator int ();
};

template <typename T> struct TPL : A
{
  using A::operator T; // { dg-error "operator float" }
};

TPL<int> i;
TPL<float> j; // { dg-error "instantiated" }
