/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl" } */
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to4\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to8\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to16\\\}" 2 } }  */

#include <immintrin.h>

volatile __m512h res0, a0, c0;
volatile __m256h res1, a1, c1;
volatile __m128h res2, a2, c2;
volatile _Float16 *b;

void extern
avx_test(void)
{
  res0 = _mm512_fmadd_pch (a0, _mm512_set1_pch(*(b + 2 * 6)), c0);
  res0 = _mm512_fcmadd_pch (a0, _mm512_set1_pch(*(b + 2 * 6)), c0);

  res1 = _mm256_fmadd_pch (a1, _mm256_set1_pch(*(b + 2 * 6)), c1);
  res1 = _mm256_fcmadd_pch (a1, _mm256_set1_pch(*(b + 2 * 6)), c1);

  res2 =  _mm_fmadd_pch (a2, _mm_set1_pch(*(b + 2 * 6)), c2);
  res2 =  _mm_fcmadd_pch (a2, _mm_set1_pch(*(b + 2 * 6)), c2);
}
