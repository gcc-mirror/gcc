// { dg-options "-w -ansi -pedantic-errors -fsigned-bitfields" }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 15 Dec 2001 <nathan@codesourcery.com>
// Split into pieces for binary compatibility testing October 2002

extern void bitfield1_x (void);

int
main ()
{
  bitfield1_x ();
}
