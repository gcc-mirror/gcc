/* { dg-do assemble { target { avx512bw && { avx512vl && { ! ia32 } } } } } */
/* { dg-options "-O2 -mavx512bw -mavx512vl" } */

#include <x86intrin.h>

void
f1 (__m128i x, __m128i y)
{
  register __m128i a __asm ("xmm16"), b __asm ("xmm17");
  a = x; b = y;
  asm volatile ("" : "+v" (a), "+v" (b));
  a = _mm_madd_epi16 (a, b);
  asm volatile ("" : "+v" (a));
}

void
f2 (__m256i x, __m256i y)
{
  register __m256i a __asm ("xmm16"), b __asm ("xmm17");
  a = x; b = y;
  asm volatile ("" : "+v" (a), "+v" (b));
  a = _mm256_madd_epi16 (a, b);
  asm volatile ("" : "+v" (a));
}
