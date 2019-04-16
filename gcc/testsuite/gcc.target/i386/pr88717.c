/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mvzeroupper" } */

#include <immintrin.h>

__m128
foo1 (__m256 x)
{
  return _mm256_castps256_ps128 (x);
}

void
foo2 (float *p, __m256 x)
{
  *p = ((__v8sf)x)[0];
}

void
foo3 (float *p, __m512 x)
{
  *p = ((__v16sf)x)[0];
}

/* { dg-final { scan-assembler-not "vzeroupper" } } */
