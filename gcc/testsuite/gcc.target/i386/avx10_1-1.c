/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=x86-64 -mavx10.1-256" } */

#include <immintrin.h>

void
f1 ()
{
  register __m256d a __asm ("ymm17");
  register __m256d b __asm ("ymm16");
  a = _mm256_add_pd (a, b);
  asm volatile ("" : "+v" (a));
}

void
f2 ()
{
  register __m128d a __asm ("xmm17");
  register __m128d b __asm ("xmm16");
  a = _mm_add_pd (a, b);
  asm volatile ("" : "+v" (a));
}
