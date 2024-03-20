/* { dg-do compile } */
/* { dg-options "-O0 -msse2 -mno-avx" } */

#include <x86intrin.h>

volatile __m128 a1, a2, a3;
volatile __m128d d1, d2, d3;

void
test (void)
{
  d1 = _mm_cmp_sd (d2, d3, 8); /* { dg-error "'__builtin_ia32_cmpsd' needs isa option -mavx" } */
  a1 = _mm_cmp_ss (a2, a3, 8); /* { dg-error "'__builtin_ia32_cmpss' needs isa option -mavx" } */
  d1 = _mm_cmp_pd (d2, d3, 8); /* { dg-error "'__builtin_ia32_cmppd' needs isa option -mavx" } */
  a1 = _mm_cmp_ps (a2, a3, 8); /* { dg-error "'__builtin_ia32_cmpps' needs isa option -mavx" } */
}
