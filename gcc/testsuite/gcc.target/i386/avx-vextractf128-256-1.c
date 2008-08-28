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
  union256d s1;
  union128d u;
  double e [2];

  s1.x = _mm256_set_pd (2134.3343,1234.635654,453.345635,54646.464356);
  u.x = _mm256_extractf128_pd (s1.x, OFFSET);

  __builtin_memcpy (e, s1.a + OFFSET * 2, sizeof e);

  if (check_union128d (u, e))
    abort ();
}
