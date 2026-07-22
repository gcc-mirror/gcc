/* PR middle-end/122715 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw -mavx512vl -fdump-tree-optimized" } */

#include <immintrin.h>

__m128i avg_u8_128(__m128i x, __m128i y)
{
  return _mm_avg_epu8 (x, y);
}

__m128i avg_u8_128_mask_all(__m128i x, __m128i y)
{
  return _mm_mask_avg_epu8 (_mm_setzero_si128 (), (__mmask16) -1, x, y);
}

__m128i avg_u8_128_mask(__m128i w, __mmask16 mask, __m128i x)
{
  return _mm_mask_avg_epu8 (w, mask, x, x);
}

/* { dg-final { scan-tree-dump-times {\.AVG_CEIL} 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times {__builtin_ia32_pavgb} 1 "optimized" } } */
/* { dg-final { scan-assembler-times {vpavgb} 3 } } */
