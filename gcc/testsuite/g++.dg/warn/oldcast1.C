// { dg-do compile }
// { dg-options "-ansi -pedantic-errors -Wold-style-cast" }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 Dec 2001 <nathan@codesourcery.com>

// PR 5089. old style cast to void should be permitted (think assert)

void foo ()
{
  int i;
  float f = (float)i;  // { dg-warning "use of old-style cast" "" }

  (void)i;
}

