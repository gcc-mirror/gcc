/* { dg-do compile} */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -mavx512bw" } */

#include <immintrin.h>

__m512h
__attribute__ ((noinline, noclone))
test_mm512_mask_blend_ph (__mmask32 U, __m512h A, __m512h B )
{
  return _mm512_mask_blend_ph (U, A, B);
}

/* { dg-final { scan-assembler-times "(?:vmovdqu16|vpblendmw)\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

__m512h
__attribute__ ((noinline, noclone))
test_mm512_permutex2var_ph (__m512h A, __m512i I, __m512h B)
{
  return _mm512_permutex2var_ph (A, I, B);
}

/* { dg-final { scan-assembler-times "vperm\[ti\]2w\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+" 1 } } */

__m512h
__attribute__ ((noinline, noclone))
test_mm512_permutexvar_ph (__m512i A, __m512h B)
{
  return _mm512_permutexvar_ph (A, B);
}

/* { dg-final { scan-assembler-times "vpermw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+" 1 } } */

__m256h
__attribute__ ((noinline, noclone))
test_mm256_mask_blend_ph (__mmask16 U, __m256h A, __m256h B )
{
  return _mm256_mask_blend_ph (U, A, B);
}

/* { dg-final { scan-assembler-times "(?:vmovdqu16|vpblendmw)\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

__m256h
__attribute__ ((noinline, noclone))
test_mm256_permutex2var_ph (__m256h A, __m256i I, __m256h B)
{
  return _mm256_permutex2var_ph (A, I, B);
}

/* { dg-final { scan-assembler-times "vperm\[ti\]2w\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+" 1 { target { ! ia32 } } } } */

__m256h
__attribute__ ((noinline, noclone))
test_mm256_permutexvar_ph (__m256i A, __m256h B)
{
  return _mm256_permutexvar_ph (A, B);
}

/* { dg-final { scan-assembler-times "vpermw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+" 1 } } */

__m128h
__attribute__ ((noinline, noclone))
test_mm_mask_blend_ph (__mmask8 U, __m128h A, __m128h B )
{
  return _mm_mask_blend_ph (U, A, B);
}

/* { dg-final { scan-assembler-times "(?:vmovdqu16|vpblendmw)\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

__m128h
__attribute__ ((noinline, noclone))
test_mm_permutex2var_ph (__m128h A, __m128i I, __m128h B)
{
  return _mm_permutex2var_ph (A, I, B);
}

/* { dg-final { scan-assembler-times "vperm\[it\]2w\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+" 1 } } */

__m128h
__attribute__ ((noinline, noclone))
test_mm_permutexvar_ph (__m128i A, __m128h B)
{
  return _mm_permutexvar_ph (A, B);
}

/* { dg-final { scan-assembler-times "vpermw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+" 1 } } */
