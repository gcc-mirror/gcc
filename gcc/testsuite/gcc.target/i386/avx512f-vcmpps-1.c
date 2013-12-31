/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler "vcmpps\[ \\t\]+\[^\n\]*\[^\}\]%zmm\[0-9\]\[^\n^k\]*%k\[1-7\]\[^\{\]" } } */
/* { dg-final { scan-assembler "vcmpps\[ \\t\]+\[^\n\]*\[^\}\]%zmm\[0-9\]\[^\n^k\]*%k\[1-7\]\{" } } */
/* { dg-final { scan-assembler "vcmpps\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]\[^\n^k\]*%k\[1-7\]\[^\{\]" } } */
/* { dg-final { scan-assembler "vcmpps\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]\[^\n^k\]*%k\[1-7\]\{" } } */

#include <immintrin.h>

volatile __m512 x;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  m = _mm512_cmp_ps_mask (x, x, _CMP_FALSE_OQ);
  m = _mm512_mask_cmp_ps_mask (m, x, x, _CMP_FALSE_OQ);
  m = _mm512_cmp_round_ps_mask (x, x, _CMP_FALSE_OQ, _MM_FROUND_NO_EXC);
  m = _mm512_mask_cmp_round_ps_mask (m, x, x, _CMP_FALSE_OQ, _MM_FROUND_NO_EXC);
}
