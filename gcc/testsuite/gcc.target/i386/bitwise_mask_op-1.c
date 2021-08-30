/* PR target/88808  */
/* { dg-do compile } */
/* { dg-options "-mavx512bw -mno-avx512dq -O2" } */

#include <immintrin.h>
__m512i
foo_orq (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask64 m1 = _mm512_cmpeq_epi8_mask (a, b);
  __mmask64 m2 = _mm512_cmpeq_epi8_mask (c, d);
  return _mm512_mask_add_epi8 (c, m1 | m2, a, d);
}

/* { dg-final { scan-assembler-times "korq" "1" { target { ! ia32 } } } } */

__m512i
foo_ord (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask32 m1 = _mm512_cmpeq_epi16_mask (a, b);
  __mmask32 m2 = _mm512_cmpeq_epi16_mask (c, d);
  return _mm512_mask_add_epi16 (c, m1 | m2, a, d);
}

/* { dg-final { scan-assembler-times "kord" "1" } }  */

__m512i
foo_orw (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask16 m1 = _mm512_cmpeq_epi32_mask (a, b);
  __mmask16 m2 = _mm512_cmpeq_epi32_mask (c, d);
  return _mm512_mask_add_epi32 (c, m1 | m2, a, d);
}

__m512i
foo_orb (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask8 m1 = _mm512_cmpeq_epi64_mask (a, b);
  __mmask8 m2 = _mm512_cmpeq_epi64_mask (c, d);
  return _mm512_mask_add_epi64 (c, m1 | m2, a, d);
}

/* { dg-final { scan-assembler-times "korw" "2" } }  */

__m512i
foo_xorq (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask64 m1 = _mm512_cmpeq_epi8_mask (a, b);
  __mmask64 m2 = _mm512_cmpeq_epi8_mask (c, d);
  return _mm512_mask_add_epi8 (c, m1 ^ m2, a, d);
}

/* { dg-final { scan-assembler-times "kxorq" "1" { target { ! ia32 } } } }  */

__m512i
foo_xord (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask32 m1 = _mm512_cmpeq_epi16_mask (a, b);
  __mmask32 m2 = _mm512_cmpeq_epi16_mask (c, d);
  return _mm512_mask_add_epi16 (c, m1 ^ m2, a, d);
}

/* { dg-final { scan-assembler-times "kxord" "1" } }  */

__m512i
foo_xorw (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask16 m1 = _mm512_cmpeq_epi32_mask (a, b);
  __mmask16 m2 = _mm512_cmpeq_epi32_mask (c, d);
  return _mm512_mask_add_epi32 (c, m1 ^ m2, a, d);
}

__m512i
foo_xorb (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask8 m1 = _mm512_cmpeq_epi64_mask (a, b);
  __mmask8 m2 = _mm512_cmpeq_epi64_mask (c, d);
  return _mm512_mask_add_epi64 (c, m1 ^ m2, a, d);
}

/* { dg-final { scan-assembler-times "korw" "2" } }  */

__m512i
foo_andq (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask64 m1 = _mm512_cmpeq_epi8_mask (a, b);
  __mmask64 m2 = _mm512_cmpeq_epi8_mask (c, d);
  return _mm512_mask_add_epi8 (c, m1 & m2, a, d);
}

__m512i
foo_andd (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask32 m1 = _mm512_cmpeq_epi16_mask (a, b);
  __mmask32 m2 = _mm512_cmpeq_epi16_mask (c, d);
  return _mm512_mask_add_epi16 (c, m1 & m2, a, d);
}

__m512i
foo_andw (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask16 m1 = _mm512_cmpeq_epi32_mask (a, b);
  __mmask16 m2 = _mm512_cmpeq_epi32_mask (c, d);
  return _mm512_mask_add_epi32 (c, m1 & m2, a, d);
}

__m512i
foo_andb (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask8 m1 = _mm512_cmpeq_epi64_mask (a, b);
  __mmask8 m2 = _mm512_cmpeq_epi64_mask (c, d);
  return _mm512_mask_add_epi64 (c, m1 & m2, a, d);
}

__m512i
foo_andnq (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask64 m1 = _mm512_cmpeq_epi8_mask (a, b);
  __mmask64 m2 = _mm512_cmpeq_epi8_mask (c, d);
  return _mm512_mask_add_epi8 (c, m1 & ~m2, a, d);
}

__m512i
foo_andnd (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask32 m1 = _mm512_cmpeq_epi16_mask (a, b);
  __mmask32 m2 = _mm512_cmpeq_epi16_mask (c, d);
  return _mm512_mask_add_epi16 (c, m1 & ~m2, a, d);
}

__m512i
foo_andnw (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask16 m1 = _mm512_cmpeq_epi32_mask (a, b);
  __mmask16 m2 = _mm512_cmpeq_epi32_mask (c, d);
  return _mm512_mask_add_epi32 (c, m1 & ~m2, a, d);
}

__m512i
foo_andnb (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask8 m1 = _mm512_cmpeq_epi64_mask (a, b);
  __mmask8 m2 = _mm512_cmpeq_epi64_mask (c, d);
  return _mm512_mask_add_epi64 (c, m1 & ~m2, a, d);
}

__m512i
foo_notq (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask64 m1 = _mm512_cmpeq_epi8_mask (a, b);
  return _mm512_mask_add_epi8 (c, ~m1, a, d);
}

__m512i
foo_notd (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask32 m1 = _mm512_cmpeq_epi16_mask (a, b);
  return _mm512_mask_add_epi16 (c, ~m1, a, d);
}

__m512i
foo_notw (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask16 m1 = _mm512_cmpeq_epi32_mask (a, b);
  return _mm512_mask_add_epi32 (c, ~m1, a, d);
}

__m512i
foo_notb (__m512i a, __m512i b, __m512i c, __m512i d)
{
  __mmask8 m1 = _mm512_cmpeq_epi64_mask (a, b);
  return _mm512_mask_add_epi64 (c, ~m1, a, d);
}
