/* { dg-require-effective-target vect_long } */
/* { dg-additional-options "-fno-tree-scev-cprop" } */

#include "tree-vect.h"

/* Statement in SLP vectorization used outside the loop.
   NOTE: SCEV disabled to ensure the live operation is not removed before
   vectorization.  */
#define LIVELOOP(RET) \
__attribute__ ((noinline)) long \
liveloop##RET (int n, long *x, long *y) \
{ \
  long n0, n1, n2, n3; \
  int j; \
  for (j = 0; j < n; ++j) \
    { \
      n0 = x[(j*4)]; \
      n1 = x[(j*4)+1]; \
      n2 = x[(j*4)+2]; \
      n3 = x[(j*4)+3]; \
      y[(j*4)] = n0 + 1; \
      y[(j*4)+1] = n1 + 2; \
      y[(j*4)+2] = n2 + 3; \
      y[(j*4)+3] = n3 + 4; \
    } \
  return n##RET; \
}

LIVELOOP (0)
LIVELOOP (1)
LIVELOOP (2)
LIVELOOP (3)
typedef long (*FP)(int n, long *x, long *y);
const FP llf[]= {&liveloop0, &liveloop1, &liveloop2, &liveloop3};

#define MAX 153

int
main (void)
{
  long a[MAX*4];
  long b[MAX*4];
  int i;

  check_vect ();

  for (i=0; i<MAX*4; i++)
    {
      __asm__ volatile ("");
      a[i] = i;
    }

  for (i=0; i<4; i++)
    {
      __asm__ volatile ("");

      int ret = llf[i] (MAX, a, b);

      if (ret != (MAX * 4) - 4 + i)
	abort ();

      for (i=0; i<MAX*4; i++)
	{
	  __asm__ volatile ("");
	  if (b[i] != i + (i%4) + 1)
	    abort ();
	}
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 4 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 4 "vect" } } */
/* { dg-final { scan-tree-dump-times "vec_stmt_relevant_p: stmt live but not relevant" 4 "vect" } } */
