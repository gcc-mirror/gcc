/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -mno-avx512bw" } */

#include <x86intrin.h>

__m128i
f1 (__m128i a, __m128i b)
{
  register __m128i c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm_packs_epi16 (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpacksswb\[^\n\r\]*xmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-not "vpacksswb\[^\n\r\]*xmm16" } } */

__m128i
f2 (__m128i a, __m128i b)
{
  register __m128i c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm_packs_epi32 (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpackssdw\[^\n\r\]*xmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-not "vpackssdw\[^\n\r\]*xmm16" } } */

__m128i
f3 (__m128i a, __m128i b)
{
  register __m128i c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm_packus_epi16 (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpackuswb\[^\n\r\]*xmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-not "vpackuswb\[^\n\r\]*xmm16" } } */

__m128i
f4 (__m128i a, __m128i b)
{
  register __m128i c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm_packus_epi32 (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpackusdw\[^\n\r\]*xmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-not "vpackusdw\[^\n\r\]*xmm16" } } */

__m256i
f5 (__m256i a, __m256i b)
{
  register __m256i c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm256_packs_epi16 (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpacksswb\[^\n\r\]*ymm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-not "vpacksswb\[^\n\r\]*ymm16" } } */

__m256i
f6 (__m256i a, __m256i b)
{
  register __m256i c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm256_packs_epi32 (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpackssdw\[^\n\r\]*ymm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-not "vpackssdw\[^\n\r\]*ymm16" } } */

__m256i
f7 (__m256i a, __m256i b)
{
  register __m256i c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm256_packus_epi16 (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpackuswb\[^\n\r\]*ymm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-not "vpackuswb\[^\n\r\]*ymm16" } } */

__m256i
f8 (__m256i a, __m256i b)
{
  register __m256i c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm256_packus_epi32 (c, b);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler-times "vpackusdw\[^\n\r\]*ymm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-not "vpackusdw\[^\n\r\]*ymm16" } } */
