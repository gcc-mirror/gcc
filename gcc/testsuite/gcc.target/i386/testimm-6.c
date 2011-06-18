/* PR target/49411 */
/* { dg-do compile } */
/* { dg-options "-O0 -mxop" } */

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
  d1 = _mm_permute2_pd (d2, d3, i1, 17);	/* { dg-error "the last argument must be a 2-bit immediate" } */
  e1 = _mm256_permute2_pd (e2, e3, l1, 17);	/* { dg-error "the last argument must be a 2-bit immediate" } */
  a1 = _mm_permute2_ps (a2, a3, i1, 17);	/* { dg-error "the last argument must be a 2-bit immediate" } */
  b1 = _mm256_permute2_ps (b2, b3, l1, 17);	/* { dg-error "the last argument must be a 2-bit immediate" } */
  d1 = _mm_permute2_pd (d2, d3, i1, k4);	/* { dg-error "the last argument must be a 2-bit immediate" } */
  e1 = _mm256_permute2_pd (e2, e3, l1, k4);	/* { dg-error "the last argument must be a 2-bit immediate" } */
  a1 = _mm_permute2_ps (a2, a3, i1, k4);	/* { dg-error "the last argument must be a 2-bit immediate" } */
  b1 = _mm256_permute2_ps (b2, b3, l1, k4);	/* { dg-error "the last argument must be a 2-bit immediate" } */
}

void
test2args (void)
{
  i1 = _mm_extracti_si64 (i2, 256, 0);		/* { dg-error "the next to last argument must be an 8-bit immediate" } */
  i1 = _mm_extracti_si64 (i2, 0, 256);		/* { dg-error "the last argument must be an 8-bit immediate" } */
  i1 = _mm_inserti_si64 (i2, i3, 256, 0);	/* { dg-error "the next to last argument must be an 8-bit immediate" } */
  i2 = _mm_inserti_si64 (i2, i3, 0, 256);	/* { dg-error "the last argument must be an 8-bit immediate" } */
  i1 = _mm_extracti_si64 (i2, k4, 0);		/* { dg-error "the next to last argument must be an 8-bit immediate" } */
  i1 = _mm_extracti_si64 (i2, 0, k4);		/* { dg-error "the last argument must be an 8-bit immediate" } */
  i1 = _mm_inserti_si64 (i2, i3, k4, 0);	/* { dg-error "the next to last argument must be an 8-bit immediate" } */
  i2 = _mm_inserti_si64 (i2, i3, 0, k4);	/* { dg-error "the last argument must be an 8-bit immediate" } */
}
