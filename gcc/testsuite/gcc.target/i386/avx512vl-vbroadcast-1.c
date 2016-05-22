/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl" } */

#include <x86intrin.h>

void
f1 (__m128d x)
{
  register __m128d a __asm ("xmm16");
  register __m256d b __asm ("xmm17");
  a = x;
  asm volatile ("" : "+v" (a));
  b = _mm256_broadcastsd_pd (a);
  asm volatile ("" : "+v" (b));
}

/* { dg-final { scan-assembler "vbroadcastsd\[^\n\r]*(xmm16\[^\n\r]*ymm17|ymm17\[^\n\r]*xmm16)" } } */

void
f2 (float const *x)
{
  register __m128 a __asm ("xmm16");
  a = _mm_broadcast_ss (x);
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler "vbroadcastss\[^\n\r]*(\\)\[^\n\r]*xmm16|xmm16\[^\n\r]*PTR)" } } */

void
f3 (float x)
{
  register float a __asm ("xmm16");
  register __m128 b __asm ("xmm17");
  a = x;
  asm volatile ("" : "+v" (a));
  float c = a;
  b = _mm_broadcast_ss (&c);
  asm volatile ("" : "+v" (b));
}

/* { dg-final { scan-assembler "vbroadcastss\[^\n\r]*xmm1\[67]\[^\n\r]*xmm1\[67]" } } */
