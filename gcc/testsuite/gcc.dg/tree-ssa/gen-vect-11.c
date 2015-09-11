/* { dg-do run { target vect_cmdline_needed } } */
/* { dg-options "-O2 -ftree-vectorize -fwrapv -fdump-tree-vect-details -fvect-cost-model=dynamic" } */
/* { dg-options "-O2 -ftree-vectorize -fwrapv -fdump-tree-vect-details -fvect-cost-model=dynamic -mno-sse" { target { i?86-*-* x86_64-*-* } } } */

#include <stdlib.h>

#define N 16

char ic[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
char ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};

int main ()
{
  int i;
  char ia[N];

  for (i = 0; i < N; i++)
    {
      ia[i] = ib[i] + ic[i];
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ia[i] != ib[i] + ic[i])
        abort ();
    }

  return 0;
}


/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { ! avr-*-* } } } } */
