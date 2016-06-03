/* { dg-require-effective-target vect_int } */
/* { dg-options "-O2 -ftree-vectorize -fno-tree-scev-cprop -fdump-tree-vect-details" } */

#include "tree-vect.h"

/* Statement that is simple and invariant used outside the loop.
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
      ret = y[0];
      x[j] = i + ret;
    }
  return ret;
}

#define MAX 77
#define START 37

int
main (void)
{
  int a[MAX];
  int b = 99;
  int i;

  int ret = liveloop (START, MAX, a, &b);

  if (ret != 99)
    abort ();

  for (i=0; i<MAX; i++)
    {
      __asm__ volatile ("");
      if (a[i] != i+START+100)
	abort ();
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-not "vec_stmt_relevant_p: stmt live but not relevant" "vect" } } */
