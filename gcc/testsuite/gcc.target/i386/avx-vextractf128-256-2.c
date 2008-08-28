/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#ifndef OFFSET
#define OFFSET 1
#endif

#if OFFSET < 0 || OFFSET > 1
#error OFFSET must be within [0..1]
#endif

void static
avx_test (void)
{
  union256 s1;
  union128 u;
  float e [4];

  s1.x = _mm256_set_ps (24.43, 68.346, 43.35, 546.46, 46.79, 82.78, 82.7, 9.4);
  u.x = _mm256_extractf128_ps (s1.x, OFFSET);

  __builtin_memcpy (e, s1.a + OFFSET * 4, sizeof e);

  if (check_union128 (u, e))
    abort ();
}
