
/* { dg-do run { target powerpc*-*-* } } */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-do run { target mipsisa64*-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -maltivec" { target powerpc*-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -msse2" { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -mpaired-single" { target mipsisa64*-*-* } } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 256

typedef float afloat __attribute__ ((__aligned__(16)));

void bar (afloat *pa, afloat *pb, afloat *pc)
{
  int i;

  /* check results:  */
  for (i = 0; i < N/2; i++)
    {
      if (pa[i] != (pb[i+1] * pc[i+1]))
	abort ();
    }

  return;
}


int
main1 (int n , afloat * __restrict__ pa, afloat * __restrict__ pb, afloat * __restrict__ pc)
{
  int i;

  for (i = 0; i < n/2; i++)
    {
      pa[i] = pb[i+1] * pc[i+1];
    }

  bar (pa,pb,pc);

  return 0;
}

int main (void)
{
  int i;
  int n=N;
  afloat a[N];
  afloat b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};
  afloat c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

  check_vect ();

  main1 (n,a,b,c);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail i?86-*-* x86_64-*-* mipsisa64*-*-* } } } */
