// { dg-do assemble  }
// { dg-options "-W " }
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 6 Febs 2001 <nathan@codesourcery.com>

// Bug 1765. We gave bogus warning on default initializer.

struct X
{
  int i;
};

X *foo ()
{
  return new X ();  // gets bogus warning
}

X x = {};           // { dg-warning "" } missing initializer
