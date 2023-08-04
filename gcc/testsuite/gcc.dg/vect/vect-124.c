/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */
#include "tree-vect.h"

#ifndef N
#define N 64
#endif

int a[N];

__attribute__((noinline, noclone)) void
foo (int x)
{
  int i;
  for (i = 0; i < N; i++, x += 3)
    a[i] = x;
}

int
main ()
{
  int i;
  
  check_vect ();
  foo (6);
#pragma GCC novector
  for (i = 0; i < N; i++)
    if (a[i] != i * 3 + 6)
      abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
