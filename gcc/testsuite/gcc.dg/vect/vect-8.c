/* { dg-do run { target powerpc*-*-* } } */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-do run { target mipsisa64*-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -maltivec" { target powerpc*-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -msse2" { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -mpaired-single" { target mipsisa64*-*-* } } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

float b[N] = {0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30};
float a[N];

int main1 (int n)
{
  int i;

  /* Vectorized: unknown loop bound).  */
  for (i = 0; i < n; i++){
    a[i] = b[i];
  }

  /* check results:  */
  for (i = 0; i < n; i++)
    {
      if (a[i] != b[i])
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 (N);
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  } } */
