// { dg-do compile }
// { dg-options "-Wall" }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 30 Dec 2003 <nathan@codesourcery.com>


// PR c++/13507, spurious warning due to attribute clobbering
extern "C" {
  extern int printf (__const char *__restrict __format, ...) throw ();
  extern int scanf (__const char *__restrict __format, ...) throw ();
}

void foo(unsigned int x)
{
  printf ("%d\n", x);
}
