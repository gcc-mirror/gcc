/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -mno-avx512dq" } */

#include <x86intrin.h>

__m128d
f1 (__m128d a, __m128d b)
{
  register __m128d c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm_and_pd (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpandq\[^\n\r\]*xmm\[0-9\]" 1 } } */

__m128d
f2 (__m128d a, __m128d b)
{
  register __m128d c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm_or_pd (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vporq\[^\n\r\]*xmm\[0-9\]" 1 } } */

__m128d
f3 (__m128d a, __m128d b)
{
  register __m128d c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm_xor_pd (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpxorq\[^\n\r\]*xmm\[0-9\]" 1 } } */

__m128d
f4 (__m128d a, __m128d b)
{
  register __m128d c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm_andnot_pd (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpandnq\[^\n\r\]*xmm\[0-9\]" 1 } } */

__m128
f5 (__m128 a, __m128 b)
{
  register __m128 c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm_and_ps (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpandd\[^\n\r\]*xmm\[0-9\]" 1 } } */

__m128
f6 (__m128 a, __m128 b)
{
  register __m128 c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm_or_ps (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpord\[^\n\r\]*xmm\[0-9\]" 1 } } */

__m128
f7 (__m128 a, __m128 b)
{
  register __m128 c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm_xor_ps (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpxord\[^\n\r\]*xmm\[0-9\]" 1 } } */

__m128
f8 (__m128 a, __m128 b)
{
  register __m128 c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm_andnot_ps (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpandnd\[^\n\r\]*xmm\[0-9\]" 1 } } */

__m256d
f9 (__m256d a, __m256d b)
{
  register __m256d c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm256_and_pd (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpandq\[^\n\r\]*ymm\[0-9\]" 1 } } */

__m256d
f10 (__m256d a, __m256d b)
{
  register __m256d c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm256_or_pd (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vporq\[^\n\r\]*ymm\[0-9\]" 1 } } */

__m256d
f11 (__m256d a, __m256d b)
{
  register __m256d c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm256_xor_pd (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpxorq\[^\n\r\]*ymm\[0-9\]" 1 } } */

__m256d
f12 (__m256d a, __m256d b)
{
  register __m256d c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm256_andnot_pd (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpandnq\[^\n\r\]*ymm\[0-9\]" 1 } } */

__m256
f13 (__m256 a, __m256 b)
{
  register __m256 c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm256_and_ps (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpandd\[^\n\r\]*ymm\[0-9\]" 1 } } */

__m256
f14 (__m256 a, __m256 b)
{
  register __m256 c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm256_or_ps (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpord\[^\n\r\]*ymm\[0-9\]" 1 } } */

__m256
f15 (__m256 a, __m256 b)
{
  register __m256 c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm256_xor_ps (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpxord\[^\n\r\]*ymm\[0-9\]" 1 } } */

__m256
f16 (__m256 a, __m256 b)
{
  register __m256 c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm256_andnot_ps (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpandnd\[^\n\r\]*ymm\[0-9\]" 1 } } */
