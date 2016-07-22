/* { dg-do assemble { target { avx512vl && { ! ia32 } } } } */
/* { dg-options "-O2 -mavx512vl" } */

#include <x86intrin.h>

void
foo (__m128i x, __m128i *y)
{
  register __m128i a __asm ("xmm16");
  a = x;
  asm volatile ("" : "+v" (a));
  a = _mm_move_epi64 (a);
  asm volatile ("" : "+v" (a));
  a = _mm_move_epi64 (*y);
  asm volatile ("" : "+v" (a));
}
