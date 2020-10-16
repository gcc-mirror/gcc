/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "(?:vmovdqu8|vinserti128)\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "(?:vmovdqu8|vextracti128)\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*\\)(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

char *p, *p1;
volatile __m256i yyy;

void extern
avx512bw_test (void)
{
  yyy = _mm256_loadu_epi8 (p);
  _mm256_storeu_epi8 (p1, yyy);
}
