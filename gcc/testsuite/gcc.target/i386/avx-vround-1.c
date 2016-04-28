/* { dg-do compile } */
/* { dg-options "-Ofast -mavx -mno-avx2" } */

#include <x86intrin.h>

__attribute__((noinline, noclone)) double
f1 (double x)
{
  return __builtin_round (x);
}

__attribute__((noinline, noclone)) float
f2 (float x)
{
  return __builtin_roundf (x);
}

__attribute__((noinline, noclone)) __m128d
f3 (__m128d x, __m128d y)
{
  return _mm_round_sd (x, y, _MM_FROUND_NINT);
}

__attribute__((noinline, noclone)) __m128
f4 (__m128 x, __m128 y)
{
  return _mm_round_ss (x, y, _MM_FROUND_NINT);
}

__attribute__((noinline, noclone)) __m128d
f5 (__m128d x)
{
  return _mm_round_pd (x, _MM_FROUND_NINT);
}

__attribute__((noinline, noclone)) __m128
f6 (__m128 x)
{
  return _mm_round_ps (x, _MM_FROUND_NINT);
}

__attribute__((noinline, noclone)) __m256d
f7 (__m256d x)
{
  return _mm256_round_pd (x, _MM_FROUND_NINT);
}

__attribute__((noinline, noclone)) __m256
f8 (__m256 x)
{
  return _mm256_round_ps (x, _MM_FROUND_NINT);
}

/* { dg-final { scan-assembler-times "vroundsd\[^\n\r\]*xmm" 2 } } */
/* { dg-final { scan-assembler-times "vroundss\[^\n\r\]*xmm" 2 } } */
/* { dg-final { scan-assembler-times "vroundpd\[^\n\r\]*xmm" 1 } } */
/* { dg-final { scan-assembler-times "vroundps\[^\n\r\]*xmm" 1 } } */
/* { dg-final { scan-assembler-times "vroundpd\[^\n\r\]*ymm" 1 } } */
/* { dg-final { scan-assembler-times "vroundps\[^\n\r\]*ymm" 1 } } */
