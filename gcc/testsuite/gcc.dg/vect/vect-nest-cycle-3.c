/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include <stdio.h>
#include "tree-vect.h"

#define N 16
#define DIFF 82

float c[N][N], b[N][N], a[N];

__attribute__ ((noinline)) int 
main1 ()
{
  int i, j;
  float diff;

  /* In inner loop vectorization -funsafe-math-optimizations is needed to 
     vectorize the summation. But in outer loop vectorization the order of
     calculation doesn't change, therefore, there is no need in that flag.  */
  for (i = 0; i < N; i++)
    {
      diff = 2;
      for (j = 0; j < N; j++) 
        diff += (b[j][i] - c[j][i]);

      a[i] = diff;
    }

  /* Check results:  */
  for (i = 0; i < N; i++)
    if (a[i] != DIFF)
      abort ();

  return 0;
}

int main (void)
{ 
  int i, j;

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      {
        b[i][j] = i+j+5;
        c[i][j] = i+j;
      }
         
  check_vect ();
  
  main1 ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
