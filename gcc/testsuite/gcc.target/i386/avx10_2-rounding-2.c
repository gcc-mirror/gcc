/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-256" } */
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
