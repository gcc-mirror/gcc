/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vcmppd\[ \\t\]+\[^\{\n\]*\[^\}\]%zmm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 9 } } */
/* { dg-final { scan-assembler-times "vcmppd\[ \\t\]+\[^\{\n\]*\[^\}\]%zmm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 9 } } */
/* { dg-final { scan-assembler-times "vcmppd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmppd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512d x;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  m = _mm512_cmp_pd_mask (x, x, _CMP_FALSE_OQ);
  m = _mm512_mask_cmp_pd_mask (m, x, x, _CMP_FALSE_OQ);
  m = _mm512_cmp_round_pd_mask (x, x, _CMP_FALSE_OQ, _MM_FROUND_NO_EXC);
  m = _mm512_mask_cmp_round_pd_mask (m, x, x, _CMP_FALSE_OQ, _MM_FROUND_NO_EXC);

  m = _mm512_cmpeq_pd_mask (x, x);
  m = _mm512_mask_cmpeq_pd_mask (m, x, x);

  m = _mm512_cmplt_pd_mask (x, x);
  m = _mm512_mask_cmplt_pd_mask (m, x, x);

  m = _mm512_cmple_pd_mask (x, x);
  m = _mm512_mask_cmple_pd_mask (m, x, x);

  m = _mm512_cmpunord_pd_mask (x, x);
  m = _mm512_mask_cmpunord_pd_mask (m, x, x);

  m = _mm512_cmpneq_pd_mask (x, x);
  m = _mm512_mask_cmpneq_pd_mask (m, x, x);

  m = _mm512_cmpnlt_pd_mask (x, x);
  m = _mm512_mask_cmpnlt_pd_mask (m, x, x);

  m = _mm512_cmpnle_pd_mask (x, x);
  m = _mm512_mask_cmpnle_pd_mask (m, x, x);

  m = _mm512_cmpord_pd_mask (x, x);
  m = _mm512_mask_cmpord_pd_mask (m, x, x);
}

