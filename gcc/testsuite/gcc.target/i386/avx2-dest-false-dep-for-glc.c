/* { dg-do compile } */
/* { dg-options "-mavx2 -mtune=generic -mtune-ctrl=dest_false_dep_for_glc -O2" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */


#include <immintrin.h>

extern __m256i i1, i2, i3, i4;
extern __m256d d1, d2;
extern __m256 f1, f2;

void vperm_test (void)
{
  i3 = _mm256_permutevar8x32_epi32 (i1, i2);
  i4 = _mm256_permute4x64_epi64 (i1, 12);
  d2 = _mm256_permute4x64_pd (d1, 12);
  f2 = _mm256_permutevar8x32_ps (f1, i2);
}

/* { dg-final { scan-assembler-times "vxorps" 4 } } */
/* { dg-final { scan-assembler-times "vpermd" 1 } } */
/* { dg-final { scan-assembler-times "vpermq" 1 } } */
/* { dg-final { scan-assembler-times "vpermpd" 1 } } */
/* { dg-final { scan-assembler-times "vpermps" 1 } } */

