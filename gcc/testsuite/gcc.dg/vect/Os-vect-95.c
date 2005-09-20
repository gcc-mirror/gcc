/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 256

void bar (float *pd, float *pa, float *pb, float *pc) 
{
  int i;

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (pa[i] != (pb[i] * pc[i]))
	abort ();
      if (pd[i] != 5.0)
	abort ();
    }

  return;
}


int
main1 (int n, float * __restrict__ pd, float * __restrict__ pa, float * __restrict__ pb, float * __restrict__ pc)
{
  int i;

  for (i = 0; i < n; i++)
    {
      pa[i] = pb[i] * pc[i];
      pd[i] = 5.0;
    }

  bar (pd,pa,pb,pc);

  return 0;
}

int main (void)
{
  int i;
  float a[N] __attribute__ ((__aligned__(16)));
  float d[N+1] __attribute__ ((__aligned__(16)));
  float b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};
  float c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

  check_vect ();

  main1 (N,&d[1],a,b,c);
  main1 (N-2,&d[1],a,b,c);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 2 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 0 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
