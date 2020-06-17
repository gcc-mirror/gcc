/* PR target/93696 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bitalg -mavx512vpopcntdq -mavx512vl -mavx512bw -masm=att" } */
/* { dg-final { scan-assembler-times "vpopcnt\[bwdq]\t%\[xyz]mm1, %\[xyz]mm0\{%k\[0-7]\}\{z\}" 12 } } */
/* { dg-final { scan-assembler-not "vmovdq\[au]\[0-9]" } } */

#include <x86intrin.h>

__m128i
f1 (__m128i x, __mmask8 m, __m128i y)
{
  return _mm_maskz_popcnt_epi64 (m, y);
}

__m128i
f2 (__m128i x, __mmask8 m, __m128i y)
{
  return _mm_maskz_popcnt_epi32 (m, y);
}

__m128i
f3 (__m128i x, __mmask8 m, __m128i y)
{
  return _mm_maskz_popcnt_epi16 (m, y);
}

__m128i
f4 (__m128i x, __mmask16 m, __m128i y)
{
  return _mm_maskz_popcnt_epi8 (m, y);
}

__m256i
f5 (__m256i x, __mmask8 m, __m256i y)
{
  return _mm256_maskz_popcnt_epi64 (m, y);
}

__m256i
f6 (__m256i x, __mmask8 m, __m256i y)
{
  return _mm256_maskz_popcnt_epi32 (m, y);
}

__m256i
f7 (__m256i x, __mmask16 m, __m256i y)
{
  return _mm256_maskz_popcnt_epi16 (m, y);
}

__m256i
f8 (__m256i x, __mmask32 m, __m256i y)
{
  return _mm256_maskz_popcnt_epi8 (m, y);
}

__m512i
f9 (__m512i x, __mmask8 m, __m512i y)
{
  return _mm512_maskz_popcnt_epi64 (m, y);
}

__m512i
f10 (__m512i x, __mmask16 m, __m512i y)
{
  return _mm512_maskz_popcnt_epi32 (m, y);
}

__m512i
f11 (__m512i x, __mmask32 m, __m512i y)
{
  return _mm512_maskz_popcnt_epi16 (m, y);
}

__m512i
f12 (__m512i x, __mmask64 m, __m512i y)
{
  return _mm512_maskz_popcnt_epi8 (m, y);
}
