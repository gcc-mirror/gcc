// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 17 Jan 2001 <nathan@codesourcery.com>

// Bug 1631. Default initialization of enumeral types did not convert to the
// enumeral type.

enum X { alpha, beta };

void f(void *ptr)
{
  X y = X ();
  X y1 (0);                   // { dg-error "" } cannot convert
  X y2 = X (0);
  X *x = new X ();
  X *x2 = new X (0);          // { dg-error "" } cannot convert
}
