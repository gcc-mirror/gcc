/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=x86-64-v3 -msm4 -mavx10.2" } */

#include <immintrin.h>

void
f1 (__m128i x, __m128i y)
{
  register __m128i a __asm("xmm16");
  register __m128i b __asm("xmm17");
  a = x;
  b = y;
  asm volatile ("" : "+v" (a), "+v" (b)); 
  a = _mm_sm4key4_epi32 (a, b);
  asm volatile ("" : "+v" (a));
}

void
f2 (__m256i x, __m256i y)
{
  register __m256i a __asm("ymm16");
  register __m256i b __asm("ymm17");
  a = x;
  b = y;
  asm volatile ("" : "+v" (a), "+v" (b)); 
  a = _mm256_sm4key4_epi32 (a, b);
  asm volatile ("" : "+v" (a));
}

void
f3 (__m128i x, __m128i y)
{
  register __m128i a __asm("xmm16");
  register __m128i b __asm("xmm17");
  a = x;
  b = y;
  asm volatile ("" : "+v" (a), "+v" (b)); 
  a = _mm_sm4rnds4_epi32 (a, b);
  asm volatile ("" : "+v" (a));
}

void
f4 (__m256i x, __m256i y)
{
  register __m256i a __asm("ymm16");
  register __m256i b __asm("ymm17");
  a = x;
  b = y;
  asm volatile ("" : "+v" (a), "+v" (b)); 
  a = _mm256_sm4rnds4_epi32 (a, b);
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler "vsm4key4\[ \\t\]+\[^\n\]*%xmm17\[^\n\]*%xmm16\[^\n\]*%xmm16" } } */
/* { dg-final { scan-assembler "vsm4key4\[ \\t\]+\[^\n\]*%ymm17\[^\n\]*%ymm16\[^\n\]*%ymm16" } } */
/* { dg-final { scan-assembler "vsm4rnds4\[ \\t\]+\[^\n\]*%xmm17\[^\n\]*%xmm16\[^\n\]*%xmm16" } } */
/* { dg-final { scan-assembler "vsm4rnds4\[ \\t\]+\[^\n\]*%ymm17\[^\n\]*%ymm16\[^\n\]*%ymm16" } } */

