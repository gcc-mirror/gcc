/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 3001


__attribute__ ((noinline))
main1 (float *pa)
{
  int i;

  for (i = 0; i < 3001; i++)
    {
      pa[i] = 2.0;
    }

  /* check results:  */
  for (i = 0; i < 3001; i++)
    {
      if (pa[i] != 2.0)
	abort ();
    }

  for (i = 1; i <= 10; i++)
    {
      pa[i] = 3.0;
    }

  /* check results:  */
  for (i = 1; i <= 10; i++)
    {
      if (pa[i] != 3.0)
	abort ();
    }
  
  return 0;
}

int main (void)
{
  int i;
  float a[N] __attribute__ ((__aligned__(16)));
  float b[N] __attribute__ ((__aligned__(16)));

  check_vect ();

  /* from bzip2: */
  for (i=0; i<N; i++) b[i] = i;
  a[0] = 0;
  for (i = 1; i <= 256; i++) a[i] = b[i-1];

  /* check results:  */
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
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 2 "vect" { target { vect_no_align && {! vector_alignment_reachable} } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 3 "vect" { xfail { vect_no_align || {! vector_alignment_reachable} } } } } */

/* in main1: */
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" { target !powerpc*-*-* !i?86-*-* !x86_64-*-* } } } */
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" { target vect_no_align } } } */

/* in main: */
/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 1 "vect" { target vect_no_align } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 1 "vect" { xfail vect_no_align } } } */

/* { dg-final { cleanup-tree-dump "vect" } } */
