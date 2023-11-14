/* PR target/112435 */
/* { dg-do assemble { target { avx512vl && { ! ia32 } } } } */
/* { dg-options "-mavx512vl -O2" } */

#include <x86intrin.h>

/* vpermf128 */
__m256
perm0 (__m256 a, __m256 b)
{
  register __m256 c __asm__("ymm17") =a;
  asm ("":"+v" (c));
  return _mm256_permute2f128_ps (c, b, 50);
}

__m256
perm1 (__m256 a, __m256 b)
{
  register __m256 c __asm__("ymm17") =a;
  asm ("":"+v" (c));
  return _mm256_permute2f128_ps (c, b, 18);
}

__m256
perm2 (__m256 a, __m256 b)
{
  register __m256 c __asm__("ymm17") =a;
  asm ("":"+v" (c));
  return _mm256_permute2f128_ps (c, b, 48);
}

__m256i
perm3 (__m256i a, __m256i b)
{
  register __m256i c __asm__("ymm17") =a;
  asm ("":"+v" (c));
  return _mm256_permute2f128_si256 (c, b, 50);
}

__m256i
perm4 (__m256i a, __m256i b)
{
  register __m256i c __asm__("ymm17") =a;
  asm ("":"+v" (c));
  return _mm256_permute2f128_si256 (c, b, 18);
}

__m256i
perm5 (__m256i a, __m256i b)
{
  register __m256i c __asm__("ymm17") =a;
  asm ("":"+v" (c));
  return _mm256_permute2f128_si256 (c, b, 48);
}

__m256d
perm6 (__m256d a, __m256d b)
{
  register __m256d c __asm__("ymm17") =a;
  asm ("":"+v" (c));
  return _mm256_permute2f128_pd (c, b, 50);
}

__m256d
perm7 (__m256d a, __m256d b)
{
  register __m256d c __asm__("ymm17") =a;
  asm ("":"+v" (c));
  return _mm256_permute2f128_pd (c, b, 18);
}

__m256d
perm8 (__m256d a, __m256d b)
{
  register __m256d c __asm__("ymm17") =a;
  asm ("":"+v" (c));
  return _mm256_permute2f128_pd (c, b, 48);
}
