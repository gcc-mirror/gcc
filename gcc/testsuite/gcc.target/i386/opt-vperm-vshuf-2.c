/* { dg-do compile } */
/* { dg-options "-Ofast -march=sapphirerapids" } */
/* { dg-final { scan-assembler-not "vmovaps" } } */
/* { dg-final { scan-assembler-not "vblendps" } } */
/* { dg-final { scan-assembler-not "vperm2i128" } } */
/* { dg-final { scan-assembler-not "vperm2f128" } } */

#include<x86intrin.h>

__m256i
perm0 (__m256i a, __m256i b)
{
  return _mm256_permute2x128_si256 (a, b, 16);
}

__m256d
perm1 (__m256d a, __m256d b)
{
  return _mm256_permute2f128_pd (a, b, 16);
}

__m256
perm2 (__m256 a, __m256 b)
{
  return _mm256_permute2f128_ps (a, b, 16);
}

__m256i
perm3 (__m256i a, __m256i b)
{
  return _mm256_permute2f128_si256 (a, b, 16);
}

__m256i
perm4 (__m256i a, __m256i b)
{
  return _mm256_permute2x128_si256 (a, b, 20);
}

__m256d
perm5 (__m256d a, __m256d b)
{
  return _mm256_permute2f128_pd (a, b, 20);
}

__m256i
perm6 (__m256i a, __m256i b)
{
  return _mm256_permute2x128_si256 (a, b, 80);
}

__m256d
perm7 (__m256d a, __m256d b)
{
  return _mm256_permute2f128_pd (a, b, 80);
}

__m256i
perm8 (__m256i a, __m256i b)
{
  return _mm256_permute2x128_si256 (a, b, 84);
}

__m256d
perm9 (__m256d a, __m256d b)
{
  return _mm256_permute2f128_pd (a, b, 84);
}
