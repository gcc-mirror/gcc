// { dg-do assemble  }
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 23 Feb 2000 <nathan@codesourcery.com>
// Derived from a bug report by Marko Maekelae <Marko.Makela@HUT.FI>


struct A;
void foo ()
{
  sizeof ( void ());        // { dg-error "" } ISO forbids
  sizeof ( void (A::*) ());
  sizeof ( void (A::*) () const);
  
  sizeof (void (*) () const); // { dg-error "" } invalid quals
  sizeof ( void () const);  // { dg-error "" } ISO forbids, ERROR - invalid quals
}
