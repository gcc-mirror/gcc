/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "--param vect-max-peeling-for-alignment=0 -fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N1 3001

#if VECTOR_BITS > 256
#define N2 (VECTOR_BITS / 32 + 2)
#else
#define N2 10
#endif

__attribute__ ((noinline)) int
main1 (float *pa)
{
  int i;

  for (i = 0; i < N1; i++)
    {
      pa[i] = 2.0;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N1; i++)
    {
      if (pa[i] != 2.0)
	abort ();
    }

#pragma GCC unroll 0
  for (i = 1; i <= N2; i++)
    {
      pa[i] = 3.0;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 1; i <= N2; i++)
    {
      if (pa[i] != 3.0)
	abort ();
    }
  
  return 0;
}

int main (void)
{
  int i;
  float a[N1] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  float b[N1] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

  check_vect ();

  /* from bzip2: */
  for (i = 0; i < N1; i++)
    b[i] = i;
  a[0] = 0;
  for (i = 1; i <= 256; i++) a[i] = b[i-1];

  /* check results:  */
#pragma GCC novector
  for (i = 1; i <= 256; i++)
    {
      if (a[i] != i-1)
	abort ();
    }
  if (a[0] != 0)
    abort ();

  main1 (a);

  return 0;
}

/* 2 loops vectorized in main1, 2 loops vectorized in main:
   the first loop in main requires vectorization of conversions,
   the second loop in main requires vectorization of misaligned load.  */

/* main && main1 together: */
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 2 "vect" { target powerpc*-*-* i?86-*-* x86_64-*-* } } } */

/* in main1: */
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" { target !powerpc*-*-* !i?86-*-* !x86_64-*-* } } } */
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" { target { vect_no_align && { ! vect_hw_misalign } } } } } */

/* in main: */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_no_align && { ! vect_hw_misalign } } } } } */

/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
