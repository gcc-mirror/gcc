/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 4
#define DOT4( a, b )  ( a[0]*b[0] + a[1]*b[1] + a[2]*b[2] + a[3]*b[3] )

int main1 (int ia[][N])
{
  int i, j;
  int ib[N] = {0,3,6,9};
  int ic[N][N];

  for (i = 0; i < N; i++)
    {
	ic[0][i] = DOT4 (ia[i], ib);
    }

  /* check results: */  
  for (i = 0; i < N; i++)
    {
       if (ic[0][i] != DOT4 (ia[i], ib))
           abort();
    }

  return 0;
}

int main (void)
{ 
  int ia[N][N] = {{1,2,3,4},{2,3,5,7},{2,4,6,8},{22,43,55,77}};

  check_vect ();

  return main1 (ia);
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "not vectorized: complicated access pattern" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
