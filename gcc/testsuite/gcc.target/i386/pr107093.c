/* { dg-do compile } */
/* { dg-options "-mavx512bw -O2 -mavx512vl" } */
/* { dg-final { scan-assembler-times {(?n)kxnor[bwqd]} 4 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times {(?n)kxnor[bwdq]} 3 { target ia32 } } }  */

#include<immintrin.h>

__m512i
foo (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask32 k1 = _mm512_cmp_epi16_mask (a, b, 1);
  __mmask32 k2 = _mm512_cmp_epi16_mask (c, d, 2);
  return _mm512_mask_mov_epi16 (a, ~(k1 ^ k2), c);
}

__m512i
foo1 (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask16 k1 = _mm512_cmp_epi32_mask (a, b, 1);
  __mmask16 k2 = _mm512_cmp_epi32_mask (c, d, 2);
  return _mm512_mask_mov_epi32 (a, ~(k1 ^ k2), c);
}

__m512i
foo2 (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask64 k1 = _mm512_cmp_epi8_mask (a, b, 1);
  __mmask64 k2 = _mm512_cmp_epi8_mask (c, d, 2);
  return _mm512_mask_mov_epi8 (a, ~(k1 ^ k2), c);
}

__m512i
foo3 (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask8 k1 = _mm512_cmp_epi64_mask (a, b, 1);
  __mmask8 k2 = _mm512_cmp_epi64_mask (c, d, 2);
  return _mm512_mask_mov_epi64 (a, ~(k1 ^ k2), c);
}
