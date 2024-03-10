/* PR target/111127 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bf16 -mavx512vl" } */
/* { dg-final { scan-assembler-times "vcvtne2ps2bf16\[ \\t\]+\[^\{\n\]*%zmm1, %zmm0, %zmm0\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ps2bf16\[ \\t\]+\[^\{\n\]*%ymm1, %ymm0, %ymm0\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ps2bf16\[ \\t\]+\[^\{\n\]*%xmm1, %xmm0, %xmm0\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

__m512bh cvttest(__mmask32 k, __m512 a, __m512 b)
{
  return _mm512_maskz_cvtne2ps_pbh (k,a,b);
}

__m256bh cvttest2(__mmask16 k, __m256 a, __m256 b)
{
  return _mm256_maskz_cvtne2ps_pbh (k,a,b);
}

__m128bh cvttest3(__mmask8 k, __m128 a, __m128 b)
{
  return _mm_maskz_cvtne2ps_pbh (k,a,b);
}

