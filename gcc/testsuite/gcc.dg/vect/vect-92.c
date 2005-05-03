/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 256

typedef float afloat __attribute__ ((__aligned__(16)));

/* known misalignment: same alignment  */

int
main1 (afloat * __restrict__ pa, afloat * __restrict__ pb, afloat * __restrict__ pc)
{
  int i;

  for (i = 0; i < 5; i++)
    {
      pa[i+1] = pb[i+1] * pc[i+1];
    }

  /* check results:  */
  for (i = 0; i < 5; i++)
    {
      if (pa[i+1] != (pb[i+1] * pc[i+1]))
	abort ();
    }

  return 0;
}

int
main2 (afloat * __restrict__ pa, afloat * __restrict__ pb, afloat * __restrict__ pc)
{
  int i;

  for (i = 0; i < 6; i++)
    {
      pa[i+1] = pb[i+1] * pc[i+1];
    }

  /* check results:  */
  for (i = 0; i < 6; i++)
    {
      if (pa[i+1] != (pb[i+1] * pc[i+1]))
	abort ();
    }

  return 0;
}

int
main3 (afloat * __restrict__ pa, afloat * __restrict__ pb, afloat * __restrict__ pc, int n)
{
  int i;

  for (i = 0; i < n; i++)
    {
      pa[i+1] = pb[i+1] * pc[i+1];
    }

  /* check results:  */
  for (i = 0; i < n; i++)
    {
      if (pa[i+1] != (pb[i+1] * pc[i+1]))
	abort ();
    }

  return 0;
}

int main (void)
{
  int i;
  afloat a[N];
  afloat b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};
  afloat c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

  check_vect ();

  main1 (a,b,c);
  main2 (a,b,c);
  main3 (a,b,c,N);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 3 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 3 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
