// { dg-do assemble  }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 28 Feb 2001 <nathan@codesourcery.com>

// Bug 338 and DR 128. Allow static cast to convert between enums.

enum E1 {e1};
enum E2 {e2};

E2 Foo (E1 e)
{
  return static_cast <E2> (e);
}
