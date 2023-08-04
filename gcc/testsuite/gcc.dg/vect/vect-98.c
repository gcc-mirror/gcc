/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define M 8
#define N 4
#define DOT4( a, b )  ( a[0]*b[0] + a[1]*b[1] + a[2]*b[2] + a[3]*b[3] )

__attribute__ ((noinline))
int main1 (int ia[][N])
{
  int i, j;
  int ib[N] = {0,3,6,9};
  int ic[M][M];

  for (i = 0; i < M; i++)
    {
	ic[0][i] = DOT4 (ia[i], ib);
    }

  /* check results: */
#pragma GCC novector
  for (i = 0; i < M; i++)
    {
       if (ic[0][i] != DOT4 (ia[i], ib))
           abort();
    }

  return 0;
}

int main (void)
{ 
  int ia[M][N] = {{1,2,3,4},{2,3,5,7},{2,4,6,8},{22,43,55,77},
		  {13,17,19,23},{29,31,37,41},{3,7,2,1},{4,9,8,3}};

  check_vect ();

  return main1 (ia);
}

/* Needs interleaving support.  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_strided4 } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" { xfail  vect_strided4 } } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
