/* { dg-require-effective-target vect_int } */
/* { dg-options "-O2 -ftree-vectorize -fno-tree-scev-cprop -fdump-tree-vect-details" } */

#include "tree-vect.h"

/* Statement in SLP vectorization used outside the loop.
   NOTE: SCEV disabled to ensure the live operation is not removed before
   vectorization.  */
#define LIVELOOP(RET) \
__attribute__ ((noinline)) int \
liveloop##RET (int n, int *x, int *y) \
{ \
  int n0, n1, j; \
  for (j = 0; j < n; ++j) \
    { \
      n0 = x[(j*2)]; \
      n1 = x[(j*2)+1]; \
      y[(j*2)] = n0 + 1; \
      y[(j*2)+1] = n1 + 2; \
    } \
  return n##RET; \
}

LIVELOOP (0)
LIVELOOP (1)
typedef int (*FP)(int n, int *x, int *y);
const FP llf[]= {&liveloop0, &liveloop1};

#define MAX 137

int
main (void)
{
  int a[MAX*4];
  int b[MAX*4];
  int i;

  for (i=0; i<MAX*2; i++)
    {
      __asm__ volatile ("");
      a[i] = i;
    }

  for (i=0; i<2; i++)
    {
      __asm__ volatile ("");

      int ret = llf[i] (MAX, a, b);

      if (ret != (MAX * 2) - 2 + i)
	abort ();

      for (i=0; i<MAX*2; i++)
	{
	  __asm__ volatile ("");
	  if (b[i] != i + (i%2) + 1)
	    abort ();
	}
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" } } */
/* { dg-final { scan-tree-dump-times "vec_stmt_relevant_p: stmt live but not relevant" 2 "vect" } } */
