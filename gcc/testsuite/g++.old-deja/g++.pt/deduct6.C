// Build don't link:
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 13 Feb 2001 <nathan@codesourcery.com>

// Bug 1962. We were not dealing with qualified array types properly.

#include <stdio.h>

template <typename T, unsigned I> int Baz (T (&obj)[I])
{
  printf ("%s\n", __PRETTY_FUNCTION__);
  return 1;
}

int main ()
{
  static int const ca[1] = {1};
  static int a[1] = {1};
  
  Baz (ca);
  Baz (a);
  
  return 0;
}
