/* { dg-do compile } */
/* { dg-options "-Ofast -march=sapphirerapids" } */
/* { dg-final { scan-assembler-times "vmovaps" 1 } } */
/* { dg-final { scan-assembler-times "vblendps\t\\\$15" 1 } } */
/* { dg-final { scan-assembler-times "vblendps\t\\\$240" 5 } } */

#include<x86intrin.h>

/* Vpermi128/Vpermf128 */
__m256i
perm0 (__m256i a, __m256i b)
{
  return _mm256_permute2x128_si256 (a, b, 50);
}

__m256i
perm1 (__m256i a, __m256i b)
{
  return _mm256_permute2x128_si256 (a, b, 18);
}

__m256i
perm2 (__m256i a, __m256i b)
{
  return _mm256_permute2x128_si256 (a, b, 48);
}

/* vshuf{i,f}{32x4,64x2} ymm .*/
__m256i
shuff0 (__m256i a, __m256i b)
{
  return _mm256_shuffle_i32x4(a, b, 2);
}

__m256
shuff1 (__m256 a, __m256 b)
{
  return _mm256_shuffle_f32x4(a, b, 2);
}

__m256i
shuff2 (__m256i a, __m256i b)
{
  return _mm256_shuffle_i64x2(a, b, 2);
}

__m256d
shuff3 (__m256d a, __m256d b)
{
  return _mm256_shuffle_f64x2(a, b, 2);
}
