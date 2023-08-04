/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

__attribute__ ((noinline)) int
main1 (void)
{
  int i;
  float a[N];

  /* Induction and type conversion.  */
  for ( i = 0; i < N; i++) 
  {
    a[i] = i;
  }

#pragma GCC novector
  for ( i = 0; i < N; i++) 
  {
    if (a[i] != i)
	abort ();
  }

  return 0;
}

int main (void)
{
  check_vect ();
  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target powerpc*-*-* i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
