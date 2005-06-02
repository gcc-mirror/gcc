// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 2 Jun 2005 <nathan@codesourcery.com>

// PR 21280
// Origin: Jens Maurer <jens.maurer@gmx.net>

#include "interface2.h"

struct A
{
  A() { }
  virtual ~A() { }
};

int main()
{
  A a;
  C<A> c(a);
}
