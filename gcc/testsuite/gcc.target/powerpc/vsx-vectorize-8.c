/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-mcpu=power7 -O2 -ftree-vectorize -fno-vect-cost-model -fdump-tree-vect-details" } */

/* Taken from vect/vect-95.c.  */
#include <stdlib.h>
#include <stdarg.h>

#define N 256

__attribute__ ((noinline))
void bar (short *pd, short *pa, short *pb, short *pc) 
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


__attribute__ ((noinline)) int
main1 (int n, short * __restrict__ pd, short * __restrict__ pa, short * __restrict__ pb, short * __restrict__ pc)
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
  short a[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  short d[N+1] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  short b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57};
  short c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

  main1 (N,&d[1],a,b,c);
  main1 (N-2,&d[1],a,b,c);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 2 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 2 "vect" } } */
