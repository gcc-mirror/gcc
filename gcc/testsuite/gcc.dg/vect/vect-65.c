/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
#define M 4

int main1 ()
{
  int i, j;
  int ib[M][M][N] = {{{0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45},
                      {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45},
		      {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45},
                      {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45}},
                     {{0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45}, 
                      {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45}, 
                      {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45},
                      {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45}},
                     {{0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45}, 
                      {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45}, 
                      {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45},
                      {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45}},
                     {{0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45}, 
                      {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45}, 
                      {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45},
                      {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45}}};
  int ia[M][M][N];
  int ic[N];	

  /* Multidimensional array. Aligned. The "inner" dimensions
     are invariant in the inner loop. Load and store. */
  for (i = 0; i < M; i++)
    {
      for (j = 0; j < N; j++)
        {
           ia[i][1][j] = ib[2][i][j];
        }
    }

  /* check results: */  
  for (i = 0; i < M; i++)
    {
      for (j = 0; j < N; j++)
        {
           if (ia[i][1][j] != ib[2][i][j])
              abort();
        }
    }

  /* Multidimensional array. Aligned. The "inner" dimensions
     are invariant in the inner loop. Load. */
  for (i = 0; i < M; i++)
    {
      for (j = 0; j < N; j++)
        {
           ic[j] = ib[2][i][j];
        }
    }

  /* check results: */
  for (i = 0; i < M; i++)
    {
      for (j = 0; j < N; j++)
        {
           if (ic[j] != ib[2][i][j])
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
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
