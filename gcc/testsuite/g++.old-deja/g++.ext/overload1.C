// { dg-do assemble  }
// { dg-options "-fpermissive" }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 28 Feb 2001 <nathan@codesourcery.com>

// Make sure we warn about our overload extension about picking the
// one with the least worse conversion

struct X
{
  X (int);
};
void Foo (int, float, bool);	// { dg-warning "" } candidate
void Foo (float, int, X);	// { dg-warning "" } candidate

void Baz ()
{
  Foo (1, 1, 0);    // { dg-warning "" } least worse
}
