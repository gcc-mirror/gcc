// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 13 Feb 2001 <nathan@codesourcery.com>

// Bug 1960. We were not dealing with qualified array types properly.

#include <stdio.h>

template <typename T> int Foo (T const *ptr)
{
  static int count = 0;
  
  printf ("%s\n", __PRETTY_FUNCTION__);
  count++;
  
  return count;
}

int main ()
{
  static int const cs = 1;
  static int const ca[1] = {1};
  static int s = 1;
  static int a[1] = {1};
  
  Foo (&cs);
  Foo (&ca);
  if (Foo (&s) != 2)
    return 1;
  if (Foo (&a) != 2)
    return 2;
  
  return 0;
}
