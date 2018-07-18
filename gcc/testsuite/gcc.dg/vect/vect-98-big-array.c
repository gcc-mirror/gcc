/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
#define DOT16( a, b)   ( a[0]*b[0]   + a[1]*b[1]   + a[2]*b[2]   + a[3]*b[3] + \
			 a[4]*b[4]   + a[5]*b[5]   + a[6]*b[6]   + a[7]*b[7] + \
			 a[8]*b[8]   + a[9]*b[9]   + a[10]*b[10] + a[11]*b[11] + \
			 a[12]*b[12] + a[13]*b[13] + a[14]*b[14] + a[15]*b[15])

__attribute__ ((noinline))
int main1 (int ia[][N])
{
  int i, j;
  int ib[N] = {0,3,6,9};
  int ic[N][N];

  for (i = 0; i < N; i++)
    {
	ic[0][i] = DOT16 (ia[i], ib);
    }

  /* check results: */
  for (i = 0; i < N; i++)
    {
       if (ic[0][i] != DOT16 (ia[i], ib))
           abort ();
    }

  return 0;
}

int main (void)
{
  int ia[N][N];
  int i,j;
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      {
        ia[i][j] = i + j + 1;
	asm volatile ("" ::: "memory");
      }

  check_vect ();

  return main1 (ia);
}

/* Needs interleaving support.  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_strided4 } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" { xfail  vect_strided4 } } } */
