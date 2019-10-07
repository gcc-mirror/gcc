/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx -mvzeroupper" } */

#include "avx-check.h"

#include <immintrin.h>

__m256i x1, x2, x3;

__attribute__ ((noinline))
static void
foo (void)
{
  x1 = x2;
}

void
bar (void)
{
  __m256i x = x1;
  foo ();
  x3 = x;
}

__attribute__ ((noinline))
void
avx_test (void)
{
  __m256i x = _mm256_set1_epi8 (3);
  x1 = x;
  bar ();
  if (__builtin_memcmp (&x3, &x, sizeof (x)))
    __builtin_abort ();
}
