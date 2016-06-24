/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

/* Two Statements used outside the loop.  SCEV cannot hoist the stmt.  */
__attribute__ ((noinline)) int
liveloop (int start, int n, int *x, int *y)
{
  int i = start;
  int j;
  int ret;

  for (j = 0; j < n; ++j)
    {
      ret = x[j] + y[j];
      i += 1;
      x[j] = i;
    }
  return ret;
}

#define MAX 173
#define START 7

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
      b[i] = i * 2;
    }

  int ret = liveloop (START, MAX, a, b);

  if (ret != (MAX - 1) * 3)
    abort ();

  for (i=0; i<MAX; i++)
    {
      __asm__ volatile ("");
      if (a[i] != i+START+1)
	abort ();
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vec_stmt_relevant_p: stmt live but not relevant" 2 "vect" } } */
