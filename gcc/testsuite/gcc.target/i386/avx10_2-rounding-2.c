/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-final { scan-assembler-times "vcvtqq2pd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\n\]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtqq2pd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\n\]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtqq2pd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\n\]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtqq2phy\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtqq2ph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtqq2ph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtqq2ps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\n\]*%ymm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtqq2ps\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\n\]*%ymm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtqq2ps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\n\]*%ymm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2dq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2dq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2dq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2qq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2qq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2qq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2udq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2udq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2udq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2uqq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2uqq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2uqq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2dq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2dq\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2dq\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2qq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2qq\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2qq\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2udq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2udq\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2udq\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2uqq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2uqq\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2uqq\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2uw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2uw\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2uw\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2w\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2w\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2w\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2dq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2dq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2dq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2qq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2qq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2qq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2udq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2udq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2udq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2uqq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2uqq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2uqq\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtudq2phy\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtudq2ph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtudq2ph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtudq2ps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtudq2ps\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtudq2ps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtuqq2pd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\n\]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtuqq2pd\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\n\]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtuqq2pd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\n\]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtuqq2phy\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtuqq2ph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtuqq2ph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtuqq2ps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\n\]*%ymm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtuqq2ps\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\n\]*%ymm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtuqq2ps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\n\]*%ymm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */

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
  xd = _mm256_cvt_roundepi64_pd (xi, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xd = _mm256_mask_cvt_roundepi64_pd (xd, m8, xi, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_cvt_roundepi64_pd (m8, xi, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  hxh = _mm256_cvt_roundepi64_ph (xi, 4);
  hxh = _mm256_mask_cvt_roundepi64_ph (hxh, m8, xi, 8);
  hxh = _mm256_maskz_cvt_roundepi64_ph (m8, xi, 11);

  hx = _mm256_cvt_roundepi64_ps (xi, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  hx = _mm256_mask_cvt_roundepi64_ps (hx, m8, xi, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  hx = _mm256_maskz_cvt_roundepi64_ps (m8, xi, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_2 (void)
{
  hxi = _mm256_cvtt_roundpd_epi32 (xd, _MM_FROUND_NO_EXC);
  hxi = _mm256_mask_cvtt_roundpd_epi32 (hxi, m8, xd, _MM_FROUND_NO_EXC);
  hxi = _mm256_maskz_cvtt_roundpd_epi32 (m8, xd, _MM_FROUND_NO_EXC);

  xi = _mm256_cvtt_roundpd_epi64 (xd, _MM_FROUND_NO_EXC);
  xi = _mm256_mask_cvtt_roundpd_epi64 (xi, m8, xd, _MM_FROUND_NO_EXC);
  xi = _mm256_maskz_cvtt_roundpd_epi64 (m8, xd, _MM_FROUND_NO_EXC);

  hxi = _mm256_cvtt_roundpd_epu32 (xd, _MM_FROUND_NO_EXC);
  hxi = _mm256_mask_cvtt_roundpd_epu32 (hxi, m8, xd, _MM_FROUND_NO_EXC);
  hxi = _mm256_maskz_cvtt_roundpd_epu32 (m8, xd, _MM_FROUND_NO_EXC);

  xi = _mm256_cvtt_roundpd_epu64 (xd, _MM_FROUND_NO_EXC);
  xi = _mm256_mask_cvtt_roundpd_epu64 (xi, m8, xd, _MM_FROUND_NO_EXC);
  xi = _mm256_maskz_cvtt_roundpd_epu64 (m8, xd, _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_3 (void)
{
  xi = _mm256_cvtt_roundph_epi32 (hxh, 4);
  xi = _mm256_mask_cvtt_roundph_epi32 (xi, m8, hxh, 8);
  xi = _mm256_maskz_cvtt_roundph_epi32 (m8, hxh, 8);

  xi = _mm256_cvtt_roundph_epi64 (hxh, 4);
  xi = _mm256_mask_cvtt_roundph_epi64 (xi, m8, hxh, 8);
  xi = _mm256_maskz_cvtt_roundph_epi64 (m8, hxh, 8);

  xi = _mm256_cvtt_roundph_epu32 (hxh, 4);
  xi = _mm256_mask_cvtt_roundph_epu32 (xi, m8, hxh, 8);
  xi = _mm256_maskz_cvtt_roundph_epu32 (m8, hxh, 8);

  xi = _mm256_cvtt_roundph_epu64 (hxh, 4);
  xi = _mm256_mask_cvtt_roundph_epu64 (xi, m8, hxh, 8);
  xi = _mm256_maskz_cvtt_roundph_epu64 (m8, hxh, 8);

  xi = _mm256_cvtt_roundph_epu16 (xh, 4);
  xi = _mm256_mask_cvtt_roundph_epu16 (xi, m16, xh, 8);
  xi = _mm256_maskz_cvtt_roundph_epu16 (m16, xh, 8);

  xi = _mm256_cvtt_roundph_epi16 (xh, 4);
  xi = _mm256_mask_cvtt_roundph_epi16 (xi, m16, xh, 8);
  xi = _mm256_maskz_cvtt_roundph_epi16 (m16, xh, 8);
}

void extern
avx10_2_test_4 (void)
{
  xi = _mm256_cvtt_roundps_epi32 (x, _MM_FROUND_NO_EXC);
  xi = _mm256_mask_cvtt_roundps_epi32 (xi, m8, x, _MM_FROUND_NO_EXC);
  xi = _mm256_maskz_cvtt_roundps_epi32 (m8, x, _MM_FROUND_NO_EXC);

  xi = _mm256_cvtt_roundps_epi64 (hx, _MM_FROUND_NO_EXC);
  xi = _mm256_mask_cvtt_roundps_epi64 (xi, m8, hx, _MM_FROUND_NO_EXC);
  xi = _mm256_maskz_cvtt_roundps_epi64 (m8, hx, _MM_FROUND_NO_EXC);

  xi = _mm256_cvtt_roundps_epu32 (x, _MM_FROUND_NO_EXC);
  xi = _mm256_mask_cvtt_roundps_epu32 (xi, m8, x, _MM_FROUND_NO_EXC);
  xi = _mm256_maskz_cvtt_roundps_epu32 (m8, x, _MM_FROUND_NO_EXC);

  xi = _mm256_cvtt_roundps_epu64 (hx, _MM_FROUND_NO_EXC);
  xi = _mm256_mask_cvtt_roundps_epu64 (xi, m8, hx, _MM_FROUND_NO_EXC);
  xi = _mm256_maskz_cvtt_roundps_epu64 (m8, hx, _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_5 (void)
{
  hxh = _mm256_cvt_roundepu32_ph (xi, 4);
  hxh = _mm256_mask_cvt_roundepu32_ph (hxh, m8, xi, 8);
  hxh = _mm256_maskz_cvt_roundepu32_ph (m8, xi, 11);

  x = _mm256_cvt_roundepu32_ps (xi, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm256_mask_cvt_roundepu32_ps (x, m8, xi, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  x = _mm256_maskz_cvt_roundepu32_ps (m8, xi, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_6 (void)
{
  xd = _mm256_cvt_roundepu64_pd (xi, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xd = _mm256_mask_cvt_roundepu64_pd (xd, m8, xi, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_cvt_roundepu64_pd (m8, xi, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  hxh = _mm256_cvt_roundepu64_ph (xi, 4);
  hxh = _mm256_mask_cvt_roundepu64_ph (hxh, m8, xi, 8);
  hxh = _mm256_maskz_cvt_roundepu64_ph (m8, xi, 11);

  hx = _mm256_cvt_roundepu64_ps (xi, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  hx = _mm256_mask_cvt_roundepu64_ps (hx, m8, xi, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  hx = _mm256_maskz_cvt_roundepu64_ps (m8, xi, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}
