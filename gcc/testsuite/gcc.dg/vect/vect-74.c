/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

typedef float afloat __attribute__ ((__aligned__(16)));

afloat a[N];
afloat b[N+4] = {0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 7.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0}; 
afloat c[N] = {0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 7.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5};

int
main1 (afloat *__restrict__  pa, afloat * __restrict__ pb, afloat * __restrict__ pc)
{
  int i;
  afloat *q = pb + 4;

  for (i = 0; i < N; i++)
    {
      pa[i] = q[i] * pc[i];
    }

  for (i = 0; i < N; i++)
    {
      if (pa[i] != q[i] * pc[i])
	abort();
    }
  
  return 0;
}

int main (void)
{ 
  check_vect ();

  main1 (a, b, c);

  return 0;	
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
