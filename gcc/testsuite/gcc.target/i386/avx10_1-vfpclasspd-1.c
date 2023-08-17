/* { dg-do compile } */
/* { dg-options "-mavx10.1 -O2" } */
/* { dg-final { scan-assembler-times "vfpclasspdy\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfpclasspdx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfpclasspdy\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfpclasspdx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256d x256;
volatile __m128d x128;
volatile __mmask8 m;

void extern
avx10_1_test (void)
{
  m = _mm256_fpclass_pd_mask (x256, 13);
  m = _mm_fpclass_pd_mask (x128, 13);
  m = _mm256_mask_fpclass_pd_mask (2, x256, 13);
  m = _mm_mask_fpclass_pd_mask (2, x128, 13);
}
