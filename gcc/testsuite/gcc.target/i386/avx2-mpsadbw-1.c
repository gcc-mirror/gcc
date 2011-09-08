/* { dg-do compile } */
/* { dg-options "-O2 -mavx2" } */
/* { dg-final { scan-assembler "mpsadbw\[ \\t\]+\[^\n\]*%ymm\[0-9\]" } } */

#include <immintrin.h>

volatile __m256i x;

void extern
avx2_test (void)
{
  /* imm = 13 is arbitrary here */
  x = _mm256_mpsadbw_epu8 (x, x, 13);
}
