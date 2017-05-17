/* PR target/80286 */
/* { dg-do run { target avx } } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"
#include <immintrin.h>

__m256i m;

__attribute__((noinline, noclone)) __m128i
foo (__m128i x)
{
  int s = _mm_cvtsi128_si32 (_mm256_castsi256_si128 (m));
  return _mm_srli_epi16 (x, s);
}

static void
avx_test (void)
{
  __m128i a = (__m128i) (__v8hi) { 1 << 7, 2 << 8, 3 << 9, 4 << 10, 5 << 11, 6 << 12, 7 << 13, 8 << 12 };
  m = (__m256i) (__v8si) { 7, 8, 9, 10, 11, 12, 13, 14 };
  __m128i c = foo (a);
  __m128i b = (__m128i) (__v8hi) { 1, 2 << 1, 3 << 2, 4 << 3, 5 << 4, 6 << 5, 7 << 6, 8 << 5 };
  if (__builtin_memcmp (&c, &b, sizeof (__m128i)))
    __builtin_abort ();
}
