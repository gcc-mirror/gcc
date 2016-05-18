/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -mavx512bw" } */

#include <x86intrin.h>

void
f1 (__m128i x, __m128i y)
{
  register __m128i a __asm ("xmm16"), b __asm ("xmm17");
  a = x;
  b = y;
  asm volatile ("" : "+v" (a), "+v" (b));
  a = _mm_shuffle_epi8 (a, b);
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler "vpshufb\[^\n\r]*xmm1\[67]\[^\n\r]*xmm1\[67]\[^\n\r]*xmm1\[67]" } } */

void
f2 (__m256i x, __m256i y)
{
  register __m256i a __asm ("xmm16"), b __asm ("xmm17");
  a = x;
  b = y;
  asm volatile ("" : "+v" (a), "+v" (b));
  a = _mm256_shuffle_epi8 (a, b);
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler "vpshufb\[^\n\r]*ymm1\[67]\[^\n\r]*ymm1\[67]\[^\n\r]*ymm1\[67]" } } */
