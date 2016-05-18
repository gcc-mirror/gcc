/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -mavx512bw" } */

#include <x86intrin.h>

void
f1 (__m128i x)
{
  register __m128i a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = _mm_broadcastb_epi8 (a);
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler "vpbroadcastb\[^\n\r]*xmm16\[^\n\r]*xmm16" } } */

void
f2 (__m128i x)
{
  register __m128i a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = _mm_broadcastw_epi16 (a);
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler "vpbroadcastw\[^\n\r]*xmm16\[^\n\r]*xmm16" } } */

void
f3 (__m128i x)
{
  register __m128i a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = _mm_broadcastd_epi32 (a);
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler "vpbroadcastd\[^\n\r]*xmm16\[^\n\r]*xmm16" } } */

void
f4 (__m128i x)
{
  register __m128i a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = _mm_broadcastq_epi64 (a);
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler "vpbroadcastq\[^\n\r]*xmm16\[^\n\r]*xmm16" } } */

void
f5 (__m128i x)
{
  register __m128i a __asm ("xmm16");
  register __m256i b __asm ("xmm17");
  a = x;
  asm volatile ("" : "+v" (a));
  b = _mm256_broadcastb_epi8 (a);
  asm volatile ("" : "+v" (b));
}

/* { dg-final { scan-assembler "vpbroadcastb\[^\n\r]*(xmm1\[67]\[^\n\r]*ymm1\[67]|ymm1\[67]\[^\n\r]*xmm1\[67])" } } */

void
f6 (__m128i x)
{
  register __m128i a __asm ("xmm16");
  register __m256i b __asm ("xmm17");
  a = x;
  asm volatile ("" : "+v" (a));
  b = _mm256_broadcastw_epi16 (a);
  asm volatile ("" : "+v" (b));
}

/* { dg-final { scan-assembler "vpbroadcastw\[^\n\r]*(xmm1\[67]\[^\n\r]*ymm1\[67]|ymm1\[67]\[^\n\r]*xmm1\[67])" } } */

void
f7 (__m128i x)
{
  register __m128i a __asm ("xmm16");
  register __m256i b __asm ("xmm17");
  a = x;
  asm volatile ("" : "+v" (a));
  b = _mm256_broadcastd_epi32 (a);
  asm volatile ("" : "+v" (b));
}

/* { dg-final { scan-assembler "vpbroadcastd\[^\n\r]*(xmm1\[67]\[^\n\r]*ymm1\[67]|ymm1\[67]\[^\n\r]*xmm1\[67])" } } */

void
f8 (__m128i x)
{
  register __m128i a __asm ("xmm16");
  register __m256i b __asm ("xmm17");
  a = x;
  asm volatile ("" : "+v" (a));
  b = _mm256_broadcastq_epi64 (a);
  asm volatile ("" : "+v" (b));
}

/* { dg-final { scan-assembler "vpbroadcastq\[^\n\r]*(xmm1\[67]\[^\n\r]*ymm1\[67]|ymm1\[67]\[^\n\r]*xmm1\[67])" } } */
