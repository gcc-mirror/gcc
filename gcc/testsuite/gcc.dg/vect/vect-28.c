/* { dg-do run { target powerpc*-*-* } } */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -maltivec" { target powerpc*-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -msse2" { target i?86-*-* x86_64-*-* } } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128
#define OFF 3

/* unaligned store.  */

int main1 (int off)
{
  int i;
  int ia[N+OFF];

  for (i = 0; i < N; i++)
    {
      ia[i+off] = 5;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ia[i+off] != 5)
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  main1 (0); /* aligned */
  main1 (OFF); /* unaligned */
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  } } */

