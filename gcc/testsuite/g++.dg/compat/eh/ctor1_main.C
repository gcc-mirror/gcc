// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 Dec 2001 <nathan@nathan@codesourcery.com>
// PR 411

// Split into pieces for binary compatibility testing October 2002

extern void ctor1_x (void);

int
main ()
{
  ctor1_x ();
}
