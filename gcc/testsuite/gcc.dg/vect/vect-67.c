/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#if VECTOR_BITS > 256
#define NINTS (VECTOR_BITS / 32)
#else
#define NINTS 8
#endif

#define N (NINTS * 2)

__attribute__ ((noinline))
int main1 (int a, int b)
{
  int i, j;
  int ia[N][4][N + NINTS];

  /* Multidimensional array. Aligned. The "inner" dimensions
     are invariant in the inner loop. Store. 
     Not vectorizable: unsupported operation. */
  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
        {
           ia[i][1][j + NINTS] = (a == b);
        }
    }

  /* check results: */  
  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
        {
           if (ia[i][1][j + NINTS] != (a == b))
              abort();
        }
    }

  return 0;
}

int main (void)
{ 
  check_vect ();

  return main1 (2 ,7);
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" { xfail { ! vect_align_stack_vars } } } } */
