/* { dg-do run { target powerpc*-*-* } } */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -maltivec" { target powerpc*-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -msse2" { target i?86-*-* x86_64-*-* } } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

int main1 (int n, int *p)
{
  int i;
  int ib[N];
  int ia[N];
  int k;

  for (i = 0; i < N; i++)
    {
      ia[i] = n;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ia[i] != n)
        abort ();
    }

  k = *p;
  for (i = 0; i < N; i++)
    {
      ib[i] = k;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ib[i] != k)
        abort ();
    }

  return 0;
}

int main (void)
{ 
  int m = 8;

  check_vect ();
  
  return main1 (m, &m);
}

/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" } } */
