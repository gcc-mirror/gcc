// { dg-options -w }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 Jul 2001 <nathan@codesourcery.com>
// Split into pieces for binary compatibility testing October 2002

// Origin stefan@space.twc.de
// Bug 3145 case 10. Horribly complicated class hierarchy

extern void vbase8_21_x (void);

int
main ()
{
  vbase8_21_x ();
}
