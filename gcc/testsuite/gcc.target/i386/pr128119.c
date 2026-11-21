/* PR rtl-optimization/128119 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse2 -masm=att -fno-stack-protector" } */

#include <emmintrin.h>

unsigned long long
foo (volatile unsigned long long *p, __m128d m)
{
  unsigned long long c = (unsigned long long) _mm_comieq_sd (m, m) | 1;
  unsigned long long v = *p;
  return ((unsigned __int128) v * c) >> 64;
}

unsigned long long
bar (unsigned long long *p, __m128d m)
{
  unsigned long long c = (unsigned long long) _mm_comieq_sd (m, m) | 1;
  unsigned long long v = __atomic_load_n (p, __ATOMIC_RELAXED);
  return ((unsigned __int128) v * c) >> 64;
}

/* { dg-final { scan-assembler-times "(?:movq|mulq)\t\\\(" 2 } } */
