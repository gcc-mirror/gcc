/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vgatherqpd\[ \\t\]+\[^\n\]*ymm\[0-9\]\[^\n\]*ymm\[0-9\]\[^\n\]*ymm\[0-9\]" } } */

#include <immintrin.h>

__m256d x;
double *base;
__m256i idx;

void extern
avx2_test (void)
{
  x = _mm256_mask_i64gather_pd (x, base, idx, x, 1);
}
