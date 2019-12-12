/* PR tree-optimization/86400 - set<string>::set<char (*)[2]) constructor
   does not work with array argument
   Verify that strlen() calls with one-character array elements of
   multidimensional arrays are still folded.
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

#include "strlenopt.h"

void f (void)
{
  extern char a[2][1];
  int n = strlen (a[1]);
  if (n >= sizeof a)
    abort();
}

void g (void)
{
  extern char b[3][2][1];
  int n = strlen (b[2][1]);
  if (n >= sizeof b)
    abort();
}

void h (void)
{
  extern char c[4][3][2][1];
  int n = strlen (c[3][2][1]);
  if (n >= sizeof c)
    abort();
}

/* { dg-final { scan-tree-dump-times "strlen1" 0 "optimized" } }
   { dg-final { scan-tree-dump-times "abort" 0 "optimized" } } */
