/* { dg-do compile } */
/* { dg-options "-O0 -msse2" } */
/* { dg-final { scan-assembler-times "cmpsd" 1 } } */
/* { dg-final { scan-assembler-times "cmpss" 1 } } */
/* { dg-final { scan-assembler-times "cmppd" 1 } } */
/* { dg-final { scan-assembler-times "cmpps" 1 } } */

#include <x86intrin.h>

__m128 a1, a2, a3, a4, a5, a6;
__m128d d1, d2, d3, d4, d5, d6;

void
test (void)
{
  d1 = _mm_cmp_sd (d2, d3, 1);
  a1 = _mm_cmp_ss (a2, a3, 2);
  d1 = _mm_cmp_pd (d2, d3, 3);
  a1 = _mm_cmp_ps (a2, a3, 4);
}
