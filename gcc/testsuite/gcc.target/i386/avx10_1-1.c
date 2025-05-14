/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=x86-64 -mavx10.1" } */
/* { dg-warning "'-mavx10.1' is aliased to 512 bit since GCC14.3 and GCC15.1 while '-mavx10.1-256' and '-mavx10.1-512' will be deprecated in GCC 16 due to all machines 512 bit vector size supported" "" { target *-*-* } 0 } */

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
