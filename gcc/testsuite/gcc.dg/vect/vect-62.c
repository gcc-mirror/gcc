/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

__attribute__ ((noinline))
int main1 ()
{
  int i, j;
  int ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  int ia[N][4][N+8];

  /* Multidimensional array. Aligned. The "inner" dimensions
     are invariant in the inner loop. Store. */
  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
        {
           ia[i][1][j+8] = ib[i];
        }
    }

  /* check results: */  
  for (i = 0; i < N; i++)
    {
#pragma GCC novector
      for (j = 0; j < N; j++)
        {
           if (ia[i][1][j+8] != ib[i])
              abort();
        }
    }

  /* Multidimensional array. Aligned. The "inner" dimensions
     are invariant in the inner loop.  The outer loop is
     vectorizable after invariant/store motion.  */
  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
        {
           ia[i][1][8] = ib[i];
        }
    }

  /* check results: */
  for (i = 0; i < N; i++)
    {
#pragma GCC novector
      for (j = 0; j < N; j++)
        {
           if (ia[i][1][8] != ib[i])
              abort();
        }
    }


  return 0;
}

int main (void)
{ 
  check_vect ();

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" { xfail { ! vect_align_stack_vars } } } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
