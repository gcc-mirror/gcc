/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-march=x86-64-v4 -O2" } */
/* { dg-final { scan-assembler-not {(?n)shr[bwl]} } } */
/* { dg-final { scan-assembler-not {(?n)movz[bw]} } } */

#include<immintrin.h>

__m512
foo (__m512d a, __m512d b, __m512 c, __m512 d)
{
  return _mm512_mask_mov_ps (c, (__mmask16) (_mm512_cmpeq_pd_mask (a, b) >> 1), d);
}


__m512i
foo1 (__m512d a, __m512d b, __m512i c, __m512i d)
{
  return _mm512_mask_mov_epi16 (c, (__mmask32) (_mm512_cmpeq_pd_mask (a, b) >> 1), d);
}

__m512i
foo2 (__m512d a, __m512d b, __m512i c, __m512i d)
{
  return _mm512_mask_mov_epi8 (c, (__mmask64) (_mm512_cmpeq_pd_mask (a, b) >> 1), d);
}

__m512i
foo3 (__m512 a, __m512 b, __m512i c, __m512i d)
{
  return _mm512_mask_mov_epi16 (c, (__mmask32) (_mm512_cmpeq_ps_mask (a, b) >> 1), d);
}

__m512i
foo4 (__m512 a, __m512 b, __m512i c, __m512i d)
{
  return _mm512_mask_mov_epi8 (c, (__mmask64) (_mm512_cmpeq_ps_mask (a, b) >> 1), d);
}

__m512i
foo5 (__m512i a, __m512i b, __m512i c, __m512i d)
{
  return _mm512_mask_mov_epi8 (c, (__mmask64) (_mm512_cmp_epi16_mask (a, b, 5) >> 1), d);
}
