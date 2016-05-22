/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -mavx512dq" } */

#include <x86intrin.h>

void
f1 (__m128i x)
{
  register __m128i a __asm ("xmm16");
  register __m256i c;
  a = x;
  asm volatile ("" : "+v" (a));
  c = _mm256_broadcastsi128_si256 (a);
  register __m256i b __asm ("xmm16");
  b = c;
  asm volatile ("" : "+v" (b));
}

/* { dg-final { scan-assembler "vinserti64x2\[^\n\r]*(xmm16\[^\n\r]*ymm16\[^\n\r]*ymm16|ymm16\[^\n\r]*ymm16\[^\n\r]*xmm16)" } } */

void
f2 (__m128i *x)
{
  register __m256i a __asm ("xmm16");
  a = _mm256_broadcastsi128_si256 (*x);
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler "vbroadcasti64x2\[^\n\r]*ymm16" } } */

void
f3 (__m128 *x)
{
  register __m256 a __asm ("xmm16");
  a = _mm256_broadcast_ps (x);
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler "vbroadcastf32x4\[^\n\r]*ymm16" } } */

void
f4 (__m128d *x)
{
  register __m256d a __asm ("xmm16");
  a = _mm256_broadcast_pd (x);
  asm volatile ("" : "+v" (a));
}

/* { dg-final { scan-assembler "vbroadcastf64x2\[^\n\r]*ymm16" } } */
