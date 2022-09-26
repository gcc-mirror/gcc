/* { dg-do run } */
/* { dg-options "-O2 -mavx2 -ftree-vectorize -fvect-cost-model=unlimited -mprefer-vector-width=256" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"
#include <string.h>
#include "pr103144-neg-1.c"

void
avx2_test (void)
{
  int* epi32_exp = (int*) malloc (N * sizeof (int));
  int* epi32_dst = (int*) malloc (N * sizeof (int));
  long long* epi64_exp = (long long*) malloc (N * sizeof (int));

  __builtin_memset (epi32_exp, 0, N * sizeof (int));
  int b = 100;

  for (int i = 0; i != N / 2; i++)
    epi64_exp[i] = ((long long) b) | (((long long) -b) << 32);
    
  memcpy (epi32_exp, epi64_exp, N * sizeof (int));
  foo_neg (epi32_dst, b);
  if (__builtin_memcmp (epi32_dst, epi32_exp, N * sizeof (int)) != 0)
    __builtin_abort ();

  foo_neg_peel (epi32_dst, b, 39);
  if (__builtin_memcmp (epi32_dst, epi32_exp, 39 * sizeof (int)) != 0)
    __builtin_abort ();

  for (int i = 0; i != N / 2; i++)
    epi64_exp[i] = ((long long) 1) | (((long long) -1) << 32);
    
  memcpy (epi32_exp, epi64_exp, N * sizeof (int));
  foo_neg_const (epi32_dst);
  if (__builtin_memcmp (epi32_dst, epi32_exp, N * sizeof (int)) != 0)
    __builtin_abort ();

  foo_neg_const_peel (epi32_dst, 39);
  if (__builtin_memcmp (epi32_dst, epi32_exp, 39 * sizeof (int)) != 0)
    __builtin_abort ();

  return;
}
