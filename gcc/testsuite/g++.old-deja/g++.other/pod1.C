// Build don't link:

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 28 Feb 2001 <nathan@codesourcery.com>

// DR 148. Now allows pointer to members in POD struct.

struct X
{
  int X::*m;
  int (X::*f) ();
};

void Foo (int, ...);

void Baz ()
{
  X x;
  
  Foo (1, x);
}
