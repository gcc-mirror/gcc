// Build don't link:
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 April 2001 <nathan@codesourcery.com>

// Bug 2510. We ICEd when a bogus char was present.

void foo ()
{
  // there is a ctrl-h on the next line
   // ERROR - stray char
}
