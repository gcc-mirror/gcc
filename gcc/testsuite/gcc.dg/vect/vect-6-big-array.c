/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

float results1[N];
float results2[N];
float a[N] = {0};
float e[N] = {0};
float b[N];
float c[N];

__attribute__ ((noinline))
int main1 ()
{
  int i;

  for (i=0; i<N; i++)
    {
      b[i] = i*3;
      c[i] = i;
      results1[i] = 0;
      results2[i] = 0;
      asm volatile ("" ::: "memory");
    }
  for (i=0; i<N/2; i++)
    {
      results1[i] = b[i+N/2] * c[i+N/2] - b[i] * c[i];
      results2[i+N/2] = b[i] * c[i+N/2] + b[i+N/2] * c[i];
      asm volatile ("" ::: "memory");
    }

  for (i = 0; i < N/2; i++)
    {
      a[i] = b[i+N/2] * c[i+N/2] - b[i] * c[i];
      e[i+N/2] = b[i] * c[i+N/2] + b[i+N/2] * c[i];
    }

  /* check results:  */
#pragma GCC novector
  for (i=0; i<N; i++)
    {
      if (a[i] != results1[i] || e[i] != results2[i])
	abort ();
    }


  for (i = 1; i <=N-4; i++)
    {
      a[i+3] = b[i-1];
    }

  /* check results:  */
#pragma GCC novector
  for (i = 1; i <=N-4; i++)
    {
      if (a[i+3] != b[i-1])
        abort ();
    }

  return 0;
}

int main (void)
{
  check_vect ();

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" { target { { vect_aligned_arrays } && {! vect_sizes_32B_16B} } } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 1 "vect" { target { {! vect_aligned_arrays } && {vect_sizes_32B_16B} } } } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
