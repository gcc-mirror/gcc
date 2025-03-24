/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-256" } */
/* { dg-final { scan-assembler-times "vaddpd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vaddpd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vaddpd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vaddph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vaddph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vaddph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vaddps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vaddps\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vaddps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcmppd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcmppd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcmpph\[ \\t\]+\\\$3\[^\n\r]*\{sae\}\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcmpph\[ \\t\]+\[^\{\n\]*\\\$4\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%k\[0-9\]\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcmpps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcmpps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */

#include <immintrin.h>

volatile __m256 x;
volatile __m256d xd;
volatile __m256h xh;
volatile __mmask8 m8;
volatile __mmask16 m16;
volatile __mmask32 m32;

void extern
avx10_2_test_1 (void)
{
  xd = _mm256_add_round_pd (xd, xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xd = _mm256_mask_add_round_pd (xd, m8, xd, xd, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_add_round_pd (m8, xd, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xh = _mm256_add_round_ph (xh, xh, 8);
  xh = _mm256_mask_add_round_ph (xh, m32, xh, xh, 8);
  xh = _mm256_maskz_add_round_ph (m32, xh, xh, 11);

  x = _mm256_add_round_ps (x, x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm256_mask_add_round_ps (x, m16, x, x, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  x = _mm256_maskz_add_round_ps (m16, x, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  m8 = _mm256_cmp_round_pd_mask (xd, xd, _CMP_FALSE_OQ, _MM_FROUND_NO_EXC);
  m8 = _mm256_mask_cmp_round_pd_mask (m8, xd, xd, _CMP_FALSE_OQ, _MM_FROUND_NO_EXC);

  m16 = _mm256_cmp_round_ph_mask (xh, xh, 3, 8);
  m16 = _mm256_mask_cmp_round_ph_mask (m16, xh, xh, 4, 4);

  m8 = _mm256_cmp_round_ps_mask (x, x, _CMP_FALSE_OQ, _MM_FROUND_NO_EXC);
  m8 = _mm256_mask_cmp_round_ps_mask (m8, x, x, _CMP_FALSE_OQ, _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_2 (void)
{
  m8 = _mm256_cmp_round_pd_mask (xd, xd, _CMP_FALSE_OQ, _MM_FROUND_NO_EXC);
  m8 = _mm256_mask_cmp_round_pd_mask (m8, xd, xd, _CMP_FALSE_OQ, _MM_FROUND_NO_EXC);

  m16 = _mm256_cmp_round_ph_mask (xh, xh, 3, 8);
  m16 = _mm256_mask_cmp_round_ph_mask (m16, xh, xh, 4, 4);

  m8 = _mm256_cmp_round_ps_mask (x, x, _CMP_FALSE_OQ, _MM_FROUND_NO_EXC);
  m8 = _mm256_mask_cmp_round_ps_mask (m8, x, x, _CMP_FALSE_OQ, _MM_FROUND_NO_EXC);
}
