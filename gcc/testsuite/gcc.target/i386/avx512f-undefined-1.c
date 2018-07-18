/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static void
avx512f_test (void)
{
  __m512 a = _mm512_undefined_ps ();
  __m512 b = _mm512_undefined ();
  __m512d c = _mm512_undefined_pd ();
  __m512i d = _mm512_undefined_epi32 ();
  __m512i e = _mm512_set1_epi32 (0);
  __m512i f = _mm512_and_epi32 ((__m512i) a, e);
  __m512i g = _mm512_and_epi32 ((__m512i) b, e);
  __m512i h = _mm512_and_epi32 ((__m512i) c, e);
  __m512i i = _mm512_and_epi32 (d, e);
  if (_mm512_cmpeq_epi32_mask (f, e) != 0xffff
      || _mm512_cmpeq_epi32_mask (g, e) != 0xffff
      || _mm512_cmpeq_epi32_mask (h, e) != 0xffff
      || _mm512_cmpeq_epi32_mask (i, e) != 0xffff)
    __builtin_abort ();
}
