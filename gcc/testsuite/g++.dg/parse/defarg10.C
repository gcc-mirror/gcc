// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 7 Jun 2005 <nathan@codesourcery.com>

// From java library.

void  Foo (int = 0);

class Klasse
{
  friend void Foo (int);
};
