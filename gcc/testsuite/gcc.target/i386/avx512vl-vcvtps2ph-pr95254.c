/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512f" } */

#include<immintrin.h>
extern __m128i res;
void
foo (__m256 a, __mmask8 m)
{
  res = _mm256_maskz_cvtps_ph (m, a, 10);
}

void
foo1 (__m128 a, __mmask8 m)
{
  res = _mm_maskz_cvtps_ph (m, a, 10);
}

/* { dg-final { scan-assembler-not "vcvtps2ph\[ \\t\]+\[^\{\n\]*%\[xy\]mm\[0-9\]\[^\n\]*res\[^\n\]*\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"} } */
