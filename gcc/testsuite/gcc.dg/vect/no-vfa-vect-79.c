/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

float fa[N] __attribute__ ((__aligned__(16)));
float fb[N+4] __attribute__ ((__aligned__(16))) = {0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 7.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0};
float fc[N] __attribute__ ((__aligned__(16))) = {0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 7.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5};

/* Like vect-80.c but the pointers are not annotated as restricted,
   and therefore can't be antialiased.  */

__attribute__ ((noinline)) int
main1 (float *pa, float *pb, float *pc)
{
  int i;
  float *q = pb + 4;

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

  main1 (fa, fb, fc);

  return 0;	
}

/* Currently the loops fail to vectorize due to aliasing problems.
  If/when the aliasing problems are resolved, unalignment may
  prevent vectorization on some targets.  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "can't determine dependence between" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
