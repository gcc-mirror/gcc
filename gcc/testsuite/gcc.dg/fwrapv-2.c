/* Copyright (C) 2003 Free Software Foundation.

   Test that the -fno-wrapv command line option is accepted and enables
   "unsafe" optimizations that rely on undefined arithmetic overflow.

   Written by Roger Sayle, 31st May 2003.  */

/* { dg-do run } */
/* { dg-options "-O2 -fno-wrapv" } */

#include <limits.h>

extern void abort ();

int test(int x)
{
  return (2*x)/2;
}

main()
{
  int x = INT_MAX;

  if (test(x) != x)
    abort ();
  return 0;
}

