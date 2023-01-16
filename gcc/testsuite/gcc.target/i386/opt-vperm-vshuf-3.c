/* { dg-do compile } */
/* { dg-options "-Ofast -march=sapphirerapids" } */
/* { dg-final { scan-assembler-times "vmov..." 3 } } */
/* { dg-final { scan-assembler-times "vblendps\t\\\$15" 3 } } */
/* { dg-final { scan-assembler-times "vblendps\t\\\$240" 3 } } */
/* { dg-final { scan-assembler-not "vperm2f128" } } */

#include<x86intrin.h>

/* Vpermf128 */
__m256
perm0 (__m256 a, __m256 b)
{
  return _mm256_permute2f128_ps (a, b, 50);
}

__m256
perm1 (__m256 a, __m256 b)
{
  return _mm256_permute2f128_ps (a, b, 18);
}

__m256
perm2 (__m256 a, __m256 b)
{
  return _mm256_permute2f128_ps (a, b, 48);
}

__m256i
perm3 (__m256i a, __m256i b)
{
  return _mm256_permute2f128_si256 (a, b, 50);
}

__m256i
perm4 (__m256i a, __m256i b)
{
  return _mm256_permute2f128_si256 (a, b, 18);
}

__m256i
perm5 (__m256i a, __m256i b)
{
  return _mm256_permute2f128_si256 (a, b, 48);
}

__m256d
perm6 (__m256d a, __m256d b)
{
  return _mm256_permute2f128_pd (a, b, 50);
}

__m256d
perm7 (__m256d a, __m256d b)
{
  return _mm256_permute2f128_pd (a, b, 18);
}

__m256d
perm8 (__m256d a, __m256d b)
{
  return _mm256_permute2f128_pd (a, b, 48);
}
