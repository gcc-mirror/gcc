// { dg-do assemble  }
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Nov 2000 <nathan@codesourcery.com>

// Bug 649. A cv qualified anonymous union would cause confusion.

struct X
{
  int fn () const
  {
    return member;
  }
  const union
  {
    int member;
  };
};
