// Build don't link:
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 23 Feb 2000 <nathan@codesourcery.com>
// Derived from a bug report by Marko Maekelae <Marko.Makela@HUT.FI>

// crash test

struct A;
void foo ()
{
  sizeof ( void ());        // ERROR - ISO forbids
  sizeof ( void (A::*) ());
  sizeof ( void (A::*) () const);
  
  sizeof (void (*) () const); // ERROR - invalid quals
  sizeof ( void () const);  // ERROR - ISO forbids, ERROR - invalid quals
}
