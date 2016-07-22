/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mf16c -mavx512vl -masm=att" } */

#include <x86intrin.h>

void
f1 (__m128 x)
{
  register __m128 a __asm ("xmm16");
  register __m128i b __asm ("xmm17");
  a = x;
  asm volatile ("" : "+v" (a));
  b = _mm_cvtps_ph (a, 1);
  asm volatile ("" : "+v" (b));
}

/* { dg-final { scan-assembler "vcvtps2ph\[^\n\r]*\\\$1\[^\n\r]*%xmm16\[^\n\r]*%xmm17" } } */

void
f2 (__m256 x)
{
  register __m256 a __asm ("xmm16");
  register __m128i b __asm ("xmm17");
  a = x;
  asm volatile ("" : "+v" (a));
  b = _mm256_cvtps_ph (a, 1);
  asm volatile ("" : "+v" (b));
}

/* { dg-final { scan-assembler "vcvtps2ph\[^\n\r]*\\\$1\[^\n\r]*%ymm16\[^\n\r]*%xmm17" } } */

void
f3 (__m256 x, __v8hi *y)
{
  register __m256 a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  *y = (__v8hi) _mm256_cvtps_ph (a, 1);
}

/* { dg-final { scan-assembler "vcvtps2ph\[^\n\r]*\\\$1\[^\n\r]*%ymm16\[^\n\r]*%\[re\]di" } } */
