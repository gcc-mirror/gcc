// { dg-do assemble  }

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 31 Mar 1999 <nathan@acm.org>

// Make sure we see through typedefs.

typedef int Int;

void fn()
{
  int *p;
  Int *&pr2 = p;
}
