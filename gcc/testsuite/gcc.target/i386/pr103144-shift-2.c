/* { dg-do run } */
/* { dg-options "-O2 -mavx2 -ftree-vectorize -fvect-cost-model=unlimited -mprefer-vector-width=256" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"
#include <string.h>
#include "pr103144-shift-1.c"

typedef int v8si __attribute__((vector_size(32)));
typedef unsigned int v8usi __attribute__((vector_size(32)));

void
avx2_test (void)
{
  int* epi32_exp = (int*) malloc (N * sizeof (int));
  int* epi32_dst = (int*) malloc (N * sizeof (int));
  unsigned int* epu32_exp = (unsigned int*) malloc (N * sizeof (int));
  unsigned int* epu32_dst = (unsigned int*) malloc (N * sizeof (int));

  __builtin_memset (epi32_exp, 0, N * sizeof (int));
  int b = 8;
  v8si init = __extension__(v8si) { b, b << 1, b << 2, b << 3, b << 4, b << 5, b << 6, b << 7 };

  for (int i = 0; i != N / 8; i++)
    {
      memcpy (epi32_exp + i * 8, &init, 32);
      init <<= 8;
    }

  foo_shl (epi32_dst, b);
  if (__builtin_memcmp (epi32_dst, epi32_exp, N * sizeof (int)) != 0)
    __builtin_abort ();

  foo_shl_peel (epi32_dst, b);
  if (__builtin_memcmp (epi32_dst, epi32_exp, 39 * sizeof (int)) != 0)
    __builtin_abort ();

  b = -11111;
  init = __extension__(v8si) { b, b >> 1, b >> 2, b >> 3, b >> 4, b >> 5, b >> 6, b >> 7 };
  for (int i = 0; i != N / 8; i++)
    {
      memcpy (epi32_exp + i * 8, &init, 32);
      init >>= 8;
    }

  foo_ashr (epi32_dst, b);
  if (__builtin_memcmp (epi32_dst, epi32_exp, N * sizeof (int)) != 0)
    __builtin_abort ();

  foo_ashr_peel (epi32_dst, b);
  if (__builtin_memcmp (epi32_dst, epi32_exp, 39 * sizeof (int)) != 0)
    {
      for (int i = 0; i != 39; i++)
	{
	  printf ("epi32_dst[%d] is %d ----", i, epi32_dst[i]);
	  printf ("epi32_exp[%d] is %d\n", i, epi32_exp[i]);
	}
         __builtin_abort ();
    }

  __builtin_memset (epu32_exp, 0, N * sizeof (int));
  unsigned int c = 11111111;
  v8usi initu = __extension__(v8usi) { c, c >> 1U, c >> 2U, c >> 3U, c >> 4U, c >> 5U, c >> 6U, c >> 7U };
  for (int i = 0; i != N / 8; i++)
    {
      memcpy (epu32_exp + i * 8, &initu, 32);
      initu >>= 8U;
    }

  foo_lshr (epu32_dst, c);
  if (__builtin_memcmp (epu32_dst, epu32_exp, N * sizeof (int)) != 0)
    __builtin_abort ();

  foo_lshr_peel (epu32_dst, c);
  if (__builtin_memcmp (epu32_dst, epu32_exp, 39 * sizeof (int)) != 0)
    __builtin_abort ();

  return;
}
