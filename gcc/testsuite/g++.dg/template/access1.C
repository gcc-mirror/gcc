// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 19 Jan 2002 <nathan@codesourcery.com>

// It is legal to specialize a template with a different class-key.

template<typename T> class X;

template<typename T> struct X<T *>
{
  int i;
};
template<> struct X<int>
{
  int i;
};

void foo ()
{
  X<int *> xip;
  X<int> xi;

  xip.i;
  xi.i;
}

