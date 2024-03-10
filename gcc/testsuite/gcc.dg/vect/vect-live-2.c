/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_long } */
/* { dg-require-effective-target vect_shift } */
/* { dg-additional-options "-fno-tree-scev-cprop" } */

#include "tree-vect.h"

/* Statement used outside the loop.
   NOTE: SCEV disabled to ensure the live operation is not removed before
   vectorization.  */
__attribute__ ((noinline)) int
liveloop (int start, int n, int *x, int *y)
{
  int i = start;
  int j;
  int ret;

  for (j = 0; j < n; ++j)
    {
      i += 1;
      x[j] = i;
      ret = y[j];
    }
  return ret;
}

#define MAX 97
#define START 13

int
main (void)
{
  int a[MAX];
  int b[MAX];
  int i;

  check_vect ();

  for (i=0; i<MAX; i++)
    {
      __asm__ volatile ("");
      b[i] = i;
    }

  int ret = liveloop (START, MAX, a, b);

  if (ret != MAX - 1)
    abort ();

#pragma GCC novector
  for (i=0; i<MAX; i++)
    {
      __asm__ volatile ("");
      if (a[i] != i+START+1)
	abort ();
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vec_stmt_relevant_p: stmt live but not relevant" 1 "vect" } } */
