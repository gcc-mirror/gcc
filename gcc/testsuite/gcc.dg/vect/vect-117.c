/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 5

static  int a[N][N] = {{ 1, 2, 3, 4, 5},
		       { 6, 7, 8, 9,10},
		       {11,12,13,14,15},
		       {16,17,18,19,20},
		       {21,22,23,24,25}};

static  int c[N][N] = {{ 1, 2, 3, 4, 5},
		       { 7, 9,11, 13,15},
		       {18,21,24,27,30},
		       {34,38,42,46,50},
		       {55,60,65,70,75}};

__attribute__ ((noinline))
int main1 (int A[N][N], int n) 
{

  int i,j;

  /* vectorizable */
  for (i = 1; i < N; i++)
  {
    for (j = 0; j < n; j++)
    {
      A[i][j] = A[i-1][j] + A[i][j];
    }
  }

  return 0;
}

int main (void)
{ 
  int i,j;

  check_vect ();

  main1 (a, N);

  /* check results: */

  for (i = 0; i < N; i++)
   {
#pragma GCC novector
    for (j = 0; j < N; j++)
     {
       if (a[i][j] != c[i][j])
         abort();
     }
  }
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "possible dependence between data-refs" 0 "vect" } } */

/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" { target { lp64 } } } } */
