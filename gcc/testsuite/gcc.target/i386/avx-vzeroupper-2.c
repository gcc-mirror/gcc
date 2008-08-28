/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static int s[4] = {234566, 8723467, 6576856, 19832468};
static int d[4] = {1,1,1,1};

static void
avx_test (void)
{
  __m128i src;

  src = _mm_loadu_si128 ((__m128i*) s);
  _mm256_zeroupper ();
  _mm_storeu_si128 ((__m128i*) d, src);

  if (__builtin_memcmp (d, s, sizeof (d)))
    abort ();
}
