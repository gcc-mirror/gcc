/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fno-tree-scev-cprop" } */

#include "tree-vect.h"

/* Statement used outside the loop.
   NOTE: SCEV disabled to ensure the live operation is not removed before
   vectorization.  */
__attribute__ ((noinline)) int
liveloop (int start, int n, int *x)
{
  int i = start;
  int j;

  for (j = 0; j < n; ++j)
    {
      i += 1;
      x[j] = i;
    }
  return i;
}

#define MAX 62
#define START 27

int
main (void)
{
  int a[MAX];
  int i;

  int ret = liveloop (START, MAX, a);

  if (ret != MAX + START)
    abort ();

  for (i=0; i<MAX; i++)
    {
      __asm__ volatile ("");
      if (a[i] != i+START+1)
	abort ();
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vec_stmt_relevant_p: stmt live but not relevant" 1 "vect" } } */
