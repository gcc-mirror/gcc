// { dg-options -w }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 20 Nov 2001 <nathan@codesourcery.com>
// Split into pieces for binary compatibility testing October 2002

// Origin stefan@space.twc.de
// Bug 3145 case 4. Horribly complicated class hierarchy

extern void vbase8_4_x (void);

int
main ()
{
  vbase8_4_x ();
}
