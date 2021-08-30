/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -mtune-ctrl=dest_false_dep_for_glc -O2" } */

#include <immintrin.h>

extern __m256h h1;
extern __m128h h2;

__mmask16 m16;
__mmask8 m8;

void complex_mul_test (void)
{
  h1 = _mm256_fmul_pch (h1, h1);
  h1 = _mm256_mask_fmul_pch (h1, m16, h1, h1);
  h1 = _mm256_maskz_fmul_pch (m16, h1, h1);
  h2 = _mm_fmul_pch (h2, h2);
  h2 = _mm_mask_fmul_pch (h2, m16, h2, h2);
  h2 = _mm_maskz_fmul_pch (m16, h2, h2);
}

/* { dg-final { scan-assembler-times "vxorps" 4 } } */
/* { dg-final { scan-assembler-times "vfmulcph" 6 } } */

