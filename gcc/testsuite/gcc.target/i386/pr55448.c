/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */

#include <immintrin.h>

static inline __m256 add1(const __m256 *a, const __m256 *b)
{
  return _mm256_add_ps(*a, *b);
}

void foo1(__m256 *a, const __m256 b)
{
  *a = add1(a, &b);
}

static inline __m128 add2(const __m128 *a, const __m128 *b)
{
  return _mm_add_ps(*a, *b);
}

void foo2(__m128 *a, const __m128 b)
{
  *a = add2(a, &b);
}

/* { dg-final { scan-assembler-not "vmovups" } } */
