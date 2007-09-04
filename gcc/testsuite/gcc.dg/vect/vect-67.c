/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

__attribute__ ((noinline))
int main1 (int a, int b)
{
  int i, j;
  int ia[N][4][N+8];

  /* Multidimensional array. Aligned. The "inner" dimensions
     are invariant in the inner loop. Store. 
     Not vectorizable: unsupported operation. */
  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
        {
           ia[i][1][j+8] = (a == b);
        }
    }

  /* check results: */  
  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
        {
           if (ia[i][1][j+8] != (a == b))
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
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
