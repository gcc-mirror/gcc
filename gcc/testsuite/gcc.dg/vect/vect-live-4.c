/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

/* Statement used outside the loop, not used inside the loop.  SCEV cannot
  hoist the stmt.  */
__attribute__ ((noinline)) int
liveloop (int n, int *x, int *y)
{
  int i;
  int ret;

  for (i = 0; i < n; ++i)
    {
      ret = x[i] + 5;
      y[i] = ret;
    }
  return ret;
}

#define MAX 273

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
      a[i] = i;
    }

  int ret = liveloop (MAX, a, b);

  if (ret != MAX + 4)
    abort ();

  for (i=0; i<MAX; i++)
    {
      __asm__ volatile ("");
      if (b[i] != i+5)
	abort ();
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vec_stmt_relevant_p: stmt live but not relevant" 1 "vect" } } */
