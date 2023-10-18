/* { dg-do run } */
/* { dg-options "-O2 -mavx2 -ftree-vectorize -fvect-cost-model=unlimited -mprefer-vector-width=256" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"
#include <string.h>
#include "pr103144-mul-1.c"

typedef int v8si __attribute__((vector_size(32)));

void
avx2_test (void)
{
  int* epi32_exp = (int*) malloc (N * sizeof (int));
  int* epi32_dst = (int*) malloc (N * sizeof (int));

  __builtin_memset (epi32_exp, 0, N * sizeof (int));
  int b = 8;
  v8si init = __extension__(v8si) { b, b * 4, b * 16, b * 64, b * 256, b * 1024, b * 4096, b * 16384 };

  for (int i = 0; i != N / 8; i++)
    {
      memcpy (epi32_exp + i * 8, &init, 32);
      init *= 65536;
    }

  foo_mul (epi32_dst, b);
  if (__builtin_memcmp (epi32_dst, epi32_exp, N * sizeof (int)) != 0)
    __builtin_abort ();

  foo_mul_peel (epi32_dst, b);
  if (__builtin_memcmp (epi32_dst, epi32_exp, 39 * 4) != 0)
    __builtin_abort ();

  init = __extension__(v8si) { 1, 4, 16, 64, 256, 1024, 4096, 16384 };
  for (int i = 0; i != N / 8; i++)
    {
      memcpy (epi32_exp + i * 8, &init, 32);
      init *= 65536;
    }

  foo_mul_const (epi32_dst);
  if (__builtin_memcmp (epi32_dst, epi32_exp, N * sizeof (int)) != 0)
    __builtin_abort ();

  foo_mul_peel_const (epi32_dst);
  if (__builtin_memcmp (epi32_dst, epi32_exp, 39 * 4) != 0)
    __builtin_abort ();

  return;
}
