/* PR target/112435 */
/* { dg-do assemble { target { avx512vl && { ! ia32 } } } } */
/* { dg-options "-mavx512vl -O2" } */

#include <x86intrin.h>

/* vpermi128/vpermf128 */
__m256i
perm0 (__m256i a, __m256i b)
{
  register __m256i c __asm__("ymm17") = a;
  asm ("":"+v" (c));
  return _mm256_permute2x128_si256 (c, b, 50);
}

__m256i
perm1 (__m256i a, __m256i b)
{
  register __m256i c __asm__("ymm17") = a;
  asm ("":"+v" (c));
  return _mm256_permute2x128_si256 (c, b, 18);
}

__m256i
perm2 (__m256i a, __m256i b)
{
  register __m256i c __asm__("ymm17") = a;
  asm ("":"+v" (c));
  return _mm256_permute2x128_si256 (c, b, 48);
}

/* vshuf{i,f}{32x4,64x2} ymm .*/
__m256i
shuff0 (__m256i a, __m256i b)
{
  register __m256i c __asm__("ymm17") = a;
  asm ("":"+v" (c));
  return _mm256_shuffle_i32x4 (c, b, 2);
}

__m256
shuff1 (__m256 a, __m256 b)
{
  register __m256 c __asm__("ymm17") = a;
  asm ("":"+v" (c));
  return _mm256_shuffle_f32x4 (c, b, 2);
}

__m256i
shuff2 (__m256i a, __m256i b)
{
  register __m256i c __asm__("ymm17") = a;
  asm ("":"+v" (c));
  return _mm256_shuffle_i64x2 (c, b, 2);
}

__m256d
shuff3 (__m256d a, __m256d b)
{
  register __m256d c __asm__("ymm17") = a;
  asm ("":"+v" (c));
  return _mm256_shuffle_f64x2 (c, b, 2);
}
