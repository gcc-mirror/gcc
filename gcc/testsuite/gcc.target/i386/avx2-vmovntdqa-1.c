/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vmovntdqa\[ \\t\]+\[^\n\]*%ymm\[0-9\]" } } */

#include <immintrin.h>

volatile __m256i x;
__m256i *y;

void extern
avx2_test (void)
{
  x = _mm256_stream_load_si256 (y);
}
