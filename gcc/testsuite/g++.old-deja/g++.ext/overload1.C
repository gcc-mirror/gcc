// Build don't link:
// Special g++ Options: -fpermissive

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 28 Feb 2001 <nathan@codesourcery.com>

// Make sure we warn about our overload extension about picking the
// one with the least worse conversion

struct X
{
  X (int);
};
void Foo (int, float, bool);	// WARNING - candidate
void Foo (float, int, X);	// WARNING - candidate

void Baz ()
{
  Foo (1, 1, 0);    // WARNING - least worse
}
