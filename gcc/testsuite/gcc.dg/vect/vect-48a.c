/* { dg-do run { target powerpc*-*-* } } */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -maltivec" { target powerpc*-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -mmmx" { target i?86-*-* x86_64-*-* } } */
  
#include <stdarg.h>
#include "tree-vect.h"

#define N 256

typedef short ashort __attribute__ ((__aligned__(16)));

void bar (short *pa, short *pb, short *pc) 
{
  int i;

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (pa[i] != (pb[i] + pc[i]))
	abort ();
    }

  return;
}


int
main1 (ashort * __restrict__ pa, short * __restrict__ pb, short * __restrict__ pc)
{
  int i;

  for (i = 0; i < N; i++)
    {
      pa[i] = pb[i] + pc[i];
    }

  bar (pa,pb,pc);

  return 0;
}

int main (void)
{
  int i;
  ashort a[N];
  ashort b[N+1] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60};
  ashort c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19};

  check_vect ();

  main1 (a,b,c);
  main1 (a,&b[1],c);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
