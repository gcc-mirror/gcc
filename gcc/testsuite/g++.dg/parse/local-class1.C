 // Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Feb 2005 <nathan@codesourcery.com>

// Origin: Jorn Wolfgang Rennecke <amylaar@gcc.gnu.org>
// Bug 19608: ICE on invalid


void f ()
{
  class c
    {
      friend void g () { } // { dg-error "local class" "" }
    };
}
