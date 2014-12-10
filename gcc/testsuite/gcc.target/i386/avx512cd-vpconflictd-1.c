/* { dg-do compile } */
/* { dg-options "-mavx512cd -O2" } */
/* { dg-final { scan-assembler-times "vpconflictd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpconflictd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpconflictd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i s;
volatile __m512i res;

void extern
avx512f_test (void)
{
  res = _mm512_conflict_epi32 (s);
  res = _mm512_mask_conflict_epi32 (res, 2, s);
  res = _mm512_maskz_conflict_epi32 (2, s);
}
