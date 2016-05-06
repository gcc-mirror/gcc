/* { dg-do assemble { target { avx512vl && { ! ia32 } } } } */
/* { dg-options "-O2 -mavx512vl" } */

#include <x86intrin.h>

void
f1 (__m128i x, int y)
{
  register __m128i a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = _mm_srai_epi32 (a, y);
  asm volatile ("" : "+v" (a));
}

void
f2 (__m128i x)
{
  register __m128i a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = _mm_srai_epi32 (a, 16);
  asm volatile ("" : "+v" (a));
}

void
f3 (__m256i x, int y)
{
  register __m256i a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = _mm256_srai_epi32 (a, y);
  asm volatile ("" : "+v" (a));
}

void
f4 (__m256i x)
{
  register __m256i a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = _mm256_srai_epi32 (a, 16);
  asm volatile ("" : "+v" (a));
}
