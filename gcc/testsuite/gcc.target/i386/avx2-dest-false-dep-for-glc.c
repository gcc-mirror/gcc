/* { dg-do compile } */
/* { dg-options "-mavx2 -mtune=generic -mtune-ctrl=dest_false_dep_for_glc -O2" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */


#include <immintrin.h>

__m256i
foo0 (__m256i i3, __m256i i1, __m256i i2)
{
  return _mm256_permutevar8x32_epi32 (i1, i2);
}

__m256i
foo1 (__m256i i2, __m256i i1)
{
  return _mm256_permute4x64_epi64 (i1, 12);
}

__m256d
foo2 (__m256d d2, __m256d d1)
{
  return _mm256_permute4x64_pd (d1, 12);
}

__m256
foo3 (__m256 f2, __m256i i2, __m256 f1)
{
  return _mm256_permutevar8x32_ps (f1, i2);
}

/* { dg-final { scan-assembler-times "vxorps" 4 } } */
/* { dg-final { scan-assembler-times "vpermd" 1 } } */
/* { dg-final { scan-assembler-times "vpermq" 1 } } */
/* { dg-final { scan-assembler-times "vpermpd" 1 } } */
/* { dg-final { scan-assembler-times "vpermps" 1 } } */

