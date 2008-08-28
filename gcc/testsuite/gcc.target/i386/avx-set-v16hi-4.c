/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static __m256i
__attribute__((noinline))
foo (short x, int i)
{
  switch (i)
    {
    case 15:
      return _mm256_set_epi16 (x, 0, 0, 0, 0, 0, 0, 0,
			       0, 0, 0, 0, 0, 0, 0, 0);
    case 14:
      return _mm256_set_epi16 (0, x, 0, 0, 0, 0, 0, 0,
			       0, 0, 0, 0, 0, 0, 0, 0);
    case 13:
      return _mm256_set_epi16 (0, 0, x, 0, 0, 0, 0, 0,
			       0, 0, 0, 0, 0, 0, 0, 0);
    case 12:
      return _mm256_set_epi16 (0, 0, 0, x, 0, 0, 0, 0,
			       0, 0, 0, 0, 0, 0, 0, 0);
    case 11:
      return _mm256_set_epi16 (0, 0, 0, 0, x, 0, 0, 0,
			       0, 0, 0, 0, 0, 0, 0, 0);
    case 10:
      return _mm256_set_epi16 (0, 0, 0, 0, 0, x, 0, 0,
			       0, 0, 0, 0, 0, 0, 0, 0);
    case 9:
      return _mm256_set_epi16 (0, 0, 0, 0, 0, 0, x, 0,
			       0, 0, 0, 0, 0, 0, 0, 0);
    case 8:
      return _mm256_set_epi16 (0, 0, 0, 0, 0, 0, 0, x,
			       0, 0, 0, 0, 0, 0, 0, 0);
    case 7:
      return _mm256_set_epi16 (0, 0, 0, 0, 0, 0, 0, 0,
			       x, 0, 0, 0, 0, 0, 0, 0);
    case 6:
      return _mm256_set_epi16 (0, 0, 0, 0, 0, 0, 0, 0,
			       0, x, 0, 0, 0, 0, 0, 0);
    case 5:
      return _mm256_set_epi16 (0, 0, 0, 0, 0, 0, 0, 0,
			       0, 0, x, 0, 0, 0, 0, 0);
    case 4:
      return _mm256_set_epi16 (0, 0, 0, 0, 0, 0, 0, 0,
			       0, 0, 0, x, 0, 0, 0, 0);
    case 3:
      return _mm256_set_epi16 (0, 0, 0, 0, 0, 0, 0, 0,
			       0, 0, 0, 0, x, 0, 0, 0);
    case 2:
      return _mm256_set_epi16 (0, 0, 0, 0, 0, 0, 0, 0,
			       0, 0, 0, 0, 0, x, 0, 0);
    case 1:
      return _mm256_set_epi16 (0, 0, 0, 0, 0, 0, 0, 0,
			       0, 0, 0, 0, 0, 0, x, 0);
    case 0:
      return _mm256_set_epi16 (0, 0, 0, 0, 0, 0, 0, 0,
			       0, 0, 0, 0, 0, 0, 0, x);
    default:
      abort ();
    }
}

static void
avx_test (void)
{
  short e = 0xbeef;
  short v[16];
  union256i_w u;
  int i, j;

  for (i = 0; i < ARRAY_SIZE (v); i++)
    {
      for (j = 0; j < ARRAY_SIZE (v); j++)
	v[j] = 0;
      v[i] = e;
      u.x = foo (e, i);
      if (check_union256i_w (u, v))
	abort ();
    }
}
