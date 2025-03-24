/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-256" } */
/* { dg-final { scan-assembler-times "vcvtuw2ph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtuw2ph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtuw2ph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtw2ph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtw2ph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtw2ph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vdivpd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vdivpd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vdivpd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vdivph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vdivph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vdivph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vdivps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vdivps\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vdivps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfcmaddcph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfcmaddcph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vfcmaddcph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfcmulcph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vfcmulcph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vfcmulcph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vfixupimmpd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1  }  } */
/* { dg-final { scan-assembler-times "vfixupimmpd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfixupimmpd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfixupimmps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1  }  } */
/* { dg-final { scan-assembler-times "vfixupimmps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfixupimmps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd...pd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd...pd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd231pd\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd...pd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd...ph\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd...ph\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd231ph\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd...ph\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd...ps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd...ps\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd231ps\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd...ps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddcph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddcph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vfmaddcph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub...pd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub...pd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub231pd\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub...pd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub...ph\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub...ph\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub231ph\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub...ph\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub...ps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub...ps\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub231ps\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub...ps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */

#include <immintrin.h>

volatile __m128 hx;
volatile __m128i hxi;
volatile __m128h hxh;
volatile __m256 x;
volatile __m256d xd;
volatile __m256h xh;
volatile __m256i xi;
volatile __mmask8 m8;
volatile __mmask16 m16;
volatile __mmask32 m32;

void extern
avx10_2_test_1 (void)
{
  xh = _mm256_cvt_roundepu16_ph (xi, 4);
  xh = _mm256_mask_cvt_roundepu16_ph (xh, m16, xi, 8);
  xh = _mm256_maskz_cvt_roundepu16_ph (m16, xi, 11);

  xh = _mm256_cvt_roundepi16_ph (xi, 4);
  xh = _mm256_mask_cvt_roundepi16_ph (xh, m16, xi, 8);
  xh = _mm256_maskz_cvt_roundepi16_ph (m16, xi, 11);
}

void extern
avx10_2_test_2 (void)
{
  xd = _mm256_div_round_pd (xd, xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xd = _mm256_mask_div_round_pd (xd, m8, xd, xd, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_div_round_pd (m8, xd, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xh = _mm256_div_round_ph (xh, xh, 8);
  xh = _mm256_mask_div_round_ph (xh, m16, xh, xh, 8);
  xh = _mm256_maskz_div_round_ph (m16, xh, xh, 11);

  x = _mm256_div_round_ps (x, x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm256_mask_div_round_ps (x, m16, x, x, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  x = _mm256_maskz_div_round_ps (m16, x, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_3 (void)
{
  xh = _mm256_fcmadd_round_pch (xh, xh, xh, 8);
  xh = _mm256_mask_fcmadd_round_pch (xh, m8, xh, xh, 8);
  xh = _mm256_mask3_fcmadd_round_pch (xh, xh, xh, m8, 8);
  xh = _mm256_maskz_fcmadd_round_pch (m8, xh, xh, xh, 11);
}

void extern
avx10_2_test_4 (void)
{
  xh = _mm256_fcmul_round_pch (xh, xh, 8);
  xh = _mm256_mask_fcmul_round_pch (xh, m8, xh, xh, 8);
  xh = _mm256_maskz_fcmul_round_pch (m8, xh, xh, 11);
}

void extern
avx10_2_test_5 (void)
{
  xh = _mm256_cmul_round_pch (xh, xh, 8);
  xh = _mm256_mask_cmul_round_pch (xh, m8, xh, xh, 8);
  xh = _mm256_maskz_cmul_round_pch (m8, xh, xh, 11);
}

void extern
avx10_2_test_6 (void)
{
  xd = _mm256_fixupimm_round_pd (xd, xd, xi, 3, _MM_FROUND_NO_EXC);
  xd = _mm256_mask_fixupimm_round_pd (xd, m8, xd, xi, 3, _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_fixupimm_round_pd (m8, xd, xd, xi, 3, _MM_FROUND_NO_EXC);

  x = _mm256_fixupimm_round_ps (x, x, xi, 3, _MM_FROUND_NO_EXC);
  x = _mm256_mask_fixupimm_round_ps (x, m8, x, xi, 3, _MM_FROUND_NO_EXC);
  x = _mm256_maskz_fixupimm_round_ps (m8, x, x, xi, 3, _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_7 (void)
{
  xd = _mm256_fmadd_round_pd (xd, xd, xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xd = _mm256_mask_fmadd_round_pd (xd, m8, xd, xd, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_mask3_fmadd_round_pd (xd, xd, xd, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_fmadd_round_pd (m8, xd, xd, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xh = _mm256_fmadd_round_ph (xh, xh, xh, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xh = _mm256_mask_fmadd_round_ph (xh, m16, xh, xh, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xh = _mm256_mask3_fmadd_round_ph (xh, xh, xh, m16, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xh = _mm256_maskz_fmadd_round_ph (m16, xh, xh, xh, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  x = _mm256_fmadd_round_ps (x, x, x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm256_mask_fmadd_round_ps (x, m8, x, x, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  x = _mm256_mask3_fmadd_round_ps (x, x, x, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  x = _mm256_maskz_fmadd_round_ps (m8, x, x, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_8 (void)
{
  xh = _mm256_fmadd_round_pch (xh, xh, xh, 8);
  xh = _mm256_mask_fmadd_round_pch (xh, m8, xh, xh, 8);
  xh = _mm256_mask3_fmadd_round_pch (xh, xh, xh, m8, 8);
  xh = _mm256_maskz_fmadd_round_pch (m8, xh, xh, xh, 11);
}

void extern
avx10_2_test_9 (void)
{
  xd = _mm256_fmaddsub_round_pd (xd, xd, xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xd = _mm256_mask_fmaddsub_round_pd (xd, m8, xd, xd, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_mask3_fmaddsub_round_pd (xd, xd, xd, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_fmaddsub_round_pd (m8, xd, xd, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xh = _mm256_fmaddsub_round_ph (xh, xh, xh, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xh = _mm256_mask_fmaddsub_round_ph (xh, m8, xh, xh, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xh = _mm256_mask3_fmaddsub_round_ph (xh, xh, xh, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xh = _mm256_maskz_fmaddsub_round_ph (m8, xh, xh, xh, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  x = _mm256_fmaddsub_round_ps (x, x, x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm256_mask_fmaddsub_round_ps (x, m8, x, x, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  x = _mm256_mask3_fmaddsub_round_ps (x, x, x, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  x = _mm256_maskz_fmaddsub_round_ps (m8, x, x, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}
