/* PR target/49411 */
/* { dg-do assemble } */
/* { dg-options "-O0 -mxop" } */
/* { dg-require-effective-target xop } */

#include <x86intrin.h>

__m128i i1, i2, i3, i4;
__m128 a1, a2, a3, a4;
__m128d d1, d2, d3, d4;
__m256i l1, l2, l3, l4;
__m256 b1, b2, b3, b4;
__m256d e1, e2, e3, e4;
__m64 m1, m2, m3, m4;
int k1, k2, k3, k4;
float f1, f2, f3, f4;

void
test2bit (void)
{
  d1 = _mm_permute2_pd (d2, d3, i1, 3);
  e1 = _mm256_permute2_pd (e2, e3, l1, 3);
  a1 = _mm_permute2_ps (a2, a3, i1, 3);
  b1 = _mm256_permute2_ps (b2, b3, l1, 3);
  d1 = _mm_permute2_pd (d2, d3, i1, 0);
  e1 = _mm256_permute2_pd (e2, e3, l1, 0);
  a1 = _mm_permute2_ps (a2, a3, i1, 0);
  b1 = _mm256_permute2_ps (b2, b3, l1, 0);
}

void
test2args (void)
{
  i1 = _mm_extracti_si64 (i2, 255, 0);
  i1 = _mm_extracti_si64 (i2, 0, 255);
  i1 = _mm_inserti_si64 (i2, i3, 255, 0);
  i2 = _mm_inserti_si64 (i2, i3, 0, 255);
  i1 = _mm_extracti_si64 (i2, 255, 255);
  i1 = _mm_extracti_si64 (i2, 255, 255);
  i1 = _mm_inserti_si64 (i2, i3, 255, 255);
  i2 = _mm_inserti_si64 (i2, i3, 255, 255);
  i1 = _mm_extracti_si64 (i2, 0, 0);
  i1 = _mm_extracti_si64 (i2, 0, 0);
  i1 = _mm_inserti_si64 (i2, i3, 0, 0);
  i2 = _mm_inserti_si64 (i2, i3, 0, 0);
}
