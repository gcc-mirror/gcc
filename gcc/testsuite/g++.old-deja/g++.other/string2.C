// { dg-do run  }
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 6 Mar 2000 <nathan@codesourcery.com>

// A char const array should never be confused for a string literal.

int main ()
{
  static const char ary[] = "wibble";
  void const *ptr = 0;
  
  ptr = ary;
  if (ptr == "wibble")
    return 1;
  if (ptr != ary)
    return 1;
  return 0;
}
