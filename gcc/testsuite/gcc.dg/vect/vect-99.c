/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include "tree-vect.h"

int ca[100];

__attribute__ ((noinline))
void foo (int n)
{
  unsigned int i;

  for (i = 0; i < n; i++)
    ca[i] = 2;
}

int main (void)
{
  int i;

  check_vect ();

  foo(100);

#pragma GCC novector
  for (i = 0; i < 100; ++i) {
    if (ca[i] != 2)
      abort();
  }
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
