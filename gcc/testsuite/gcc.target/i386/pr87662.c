/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl" } */
/* { dg-final { scan-assembler-times "vpord\[^\n\r\]*ymm16" 1 } } */
/* { dg-final { scan-assembler-times "vpord\[^\n\r\]*xmm16" 1 } } */
/* { dg-final { scan-assembler-times "vporq\[^\n\r\]*ymm16" 1 } } */
/* { dg-final { scan-assembler-times "vporq\[^\n\r\]*xmm16" 1 } } */
/* { dg-final { scan-assembler-times "vpxord\[^\n\r\]*ymm16" 1 } } */
/* { dg-final { scan-assembler-times "vpxord\[^\n\r\]*xmm16" 1 } } */
/* { dg-final { scan-assembler-times "vpxorq\[^\n\r\]*ymm16" 1 } } */
/* { dg-final { scan-assembler-times "vpxorq\[^\n\r\]*xmm16" 1 } } */

#include <immintrin.h>

__m256i
foo1 (__m256i x, __m256i y)
{
  register __m256i z __asm ("xmm16") = y;
  asm volatile ("" : "+v" (z));
  return _mm256_or_epi32 (x, z);
}

__m256i
foo2 (__m256i x, __m256i y)
{
  register __m256i z __asm ("xmm16") = y;
  asm volatile ("" : "+v" (z));
  return _mm256_xor_epi32 (x, z);
}

__m128i
foo3 (__m128i x, __m128i y)
{
  register __m128i z __asm ("xmm16") = y;
  asm volatile ("" : "+v" (z));
  return _mm_or_epi32 (x, z);
}

__m128i
foo4 (__m128i x, __m128i y)
{
  register __m128i z __asm ("xmm16") = y;
  asm volatile ("" : "+v" (z));
  return _mm_xor_epi32 (x, z);
}

__m256i
foo5 (__m256i x, __m256i y)
{
  register __m256i z __asm ("xmm16") = y;
  asm volatile ("" : "+v" (z));
  return _mm256_or_epi64 (x, z);
}

__m256i
foo6 (__m256i x, __m256i y)
{
  register __m256i z __asm ("xmm16") = y;
  asm volatile ("" : "+v" (z));
  return _mm256_xor_epi64 (x, z);
}

__m128i
foo7 (__m128i x, __m128i y)
{
  register __m128i z __asm ("xmm16") = y;
  asm volatile ("" : "+v" (z));
  return _mm_or_epi64 (x, z);
}

__m128i
foo8 (__m128i x, __m128i y)
{
  register __m128i z __asm ("xmm16") = y;
  asm volatile ("" : "+v" (z));
  return _mm_xor_epi64 (x, z);
}
