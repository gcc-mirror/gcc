/* PR tree-optimization/86400 - set<string>::set<char (*)[2]) constructor
   does not work with array argument
   Verify that strlen() calls with two-character array elements of
   multidimensional arrays whose higher order dimension is 1 are not
   folded.
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

#include "strlenopt.h"

void f (void)
{
  extern char a[1][2];
  int n = strlen (*a);
  if (n != 1)
    abort();
}

void g (void)
{
  extern char b[1][2];
  int n = strlen (b[0]);
  if (n != 1)
    abort();
}

void h (void)
{
  extern char c[1][2];
  int n = strlen (&c[0][0]);
  if (n != 1)
    abort();
}

/* { dg-final { scan-tree-dump-times "= strlen" 3 "optimized" } }
   { dg-final { scan-tree-dump-times "abort" 3 "optimized" } } */
