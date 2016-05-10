/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mno-avx512dq" } */

#include <x86intrin.h>

__m128d
f1 (__m128d a, __m128d b)
{
  return _mm_and_pd (a, b);
}

/* { dg-final { scan-assembler-times "vandpd\[^\n\r\]*xmm\[0-9\]" 1 } } */

__m128d
f2 (__m128d a, __m128d b)
{
  return _mm_or_pd (a, b);
}

/* { dg-final { scan-assembler-times "vorpd\[^\n\r\]*xmm\[0-9\]" 1 } } */

__m128d
f3 (__m128d a, __m128d b)
{
  return _mm_xor_pd (a, b);
}

/* { dg-final { scan-assembler-times "vxorpd\[^\n\r\]*xmm\[0-9\]" 1 } } */

__m128d
f4 (__m128d a, __m128d b)
{
  return _mm_andnot_pd (a, b);
}

/* { dg-final { scan-assembler-times "vandnpd\[^\n\r\]*xmm\[0-9\]" 1 } } */

__m128
f5 (__m128 a, __m128 b)
{
  return _mm_and_ps (a, b);
}

/* { dg-final { scan-assembler-times "vandps\[^\n\r\]*xmm\[0-9\]" 1 } } */

__m128
f6 (__m128 a, __m128 b)
{
  return _mm_or_ps (a, b);
}

/* { dg-final { scan-assembler-times "vorps\[^\n\r\]*xmm\[0-9\]" 1 } } */

__m128
f7 (__m128 a, __m128 b)
{
  return _mm_xor_ps (a, b);
}

/* { dg-final { scan-assembler-times "vxorps\[^\n\r\]*xmm\[0-9\]" 1 } } */

__m128
f8 (__m128 a, __m128 b)
{
  return _mm_andnot_ps (a, b);
}

/* { dg-final { scan-assembler-times "vandnps\[^\n\r\]*xmm\[0-9\]" 1 } } */

__m256d
f9 (__m256d a, __m256d b)
{
  return _mm256_and_pd (a, b);
}

/* { dg-final { scan-assembler-times "vandpd\[^\n\r\]*ymm\[0-9\]" 1 } } */

__m256d
f10 (__m256d a, __m256d b)
{
  return _mm256_or_pd (a, b);
}

/* { dg-final { scan-assembler-times "vorpd\[^\n\r\]*ymm\[0-9\]" 1 } } */

__m256d
f11 (__m256d a, __m256d b)
{
  return _mm256_xor_pd (a, b);
}

/* { dg-final { scan-assembler-times "vxorpd\[^\n\r\]*ymm\[0-9\]" 1 } } */

__m256d
f12 (__m256d a, __m256d b)
{
  return _mm256_andnot_pd (a, b);
}

/* { dg-final { scan-assembler-times "vandnpd\[^\n\r\]*ymm\[0-9\]" 1 } } */

__m256
f13 (__m256 a, __m256 b)
{
  return _mm256_and_ps (a, b);
}

/* { dg-final { scan-assembler-times "vandps\[^\n\r\]*ymm\[0-9\]" 1 } } */

__m256
f14 (__m256 a, __m256 b)
{
  return _mm256_or_ps (a, b);
}

/* { dg-final { scan-assembler-times "vorps\[^\n\r\]*ymm\[0-9\]" 1 } } */

__m256
f15 (__m256 a, __m256 b)
{
  return _mm256_xor_ps (a, b);
}

/* { dg-final { scan-assembler-times "vxorps\[^\n\r\]*ymm\[0-9\]" 1 } } */

__m256
f16 (__m256 a, __m256 b)
{
  return _mm256_andnot_ps (a, b);
}

/* { dg-final { scan-assembler-times "vandnps\[^\n\r\]*ymm\[0-9\]" 1 } } */
