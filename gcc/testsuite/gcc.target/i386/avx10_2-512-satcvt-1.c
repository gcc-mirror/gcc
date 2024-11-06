/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-512" } */
/* { dg-final { scan-assembler-times "vcvtph2ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2ibs\[ \\t\]+\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2ibs\[ \\t\]+\{rz-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2iubs\[ \\t\]+\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2iubs\[ \\t\]+\{rz-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2ibs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2ibs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2iubs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2iubs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2ibs\[ \\t\]+\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2ibs\[ \\t\]+\{rz-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2iubs\[ \\t\]+\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2iubs\[ \\t\]+\{rz-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2ibs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2ibs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2iubs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2iubs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtnebf162ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtnebf162ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtnebf162ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtnebf162iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtnebf162iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtnebf162iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttnebf162ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttnebf162ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttnebf162ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttnebf162iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttnebf162iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttnebf162iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2dqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2dqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2dqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2qqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2qqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2qqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2udqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2udqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2udqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2uqqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2uqqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2uqqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2dqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2dqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2dqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2qqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2qqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2qqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2udqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2udqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2udqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2uqqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2uqqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2uqqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */

#include <immintrin.h>

volatile __m256 hx;
volatile __m256i hxi;
volatile __m512 x;
volatile __m512h xh;
volatile __m512i xi;
volatile __m512d xd;
volatile __m512bh xbh;
volatile __mmask8 m8;
volatile __mmask16 m16;
volatile __mmask32 m32;

void extern
avx10_2_test (void)
{
  xi = _mm512_ipcvt_roundph_epi16 (xh, 4);
  xi = _mm512_mask_ipcvt_roundph_epi16 (xi, m32, xh, 8);
  xi = _mm512_maskz_ipcvt_roundph_epi16 (m32, xh, 11);

  xi = _mm512_ipcvt_roundph_epu16 (xh, 4);
  xi = _mm512_mask_ipcvt_roundph_epu16 (xi, m32, xh, 8);
  xi = _mm512_maskz_ipcvt_roundph_epu16 (m32, xh, 11);

  xi = _mm512_ipcvtt_roundph_epi16 (xh, 4);
  xi = _mm512_mask_ipcvtt_roundph_epi16 (xi, m32, xh, 8);
  xi = _mm512_maskz_ipcvtt_roundph_epi16 (m32, xh, 8);

  xi = _mm512_ipcvtt_roundph_epu16 (xh, 4);
  xi = _mm512_mask_ipcvtt_roundph_epu16 (xi, m32, xh, 8);
  xi = _mm512_maskz_ipcvtt_roundph_epu16 (m32, xh, 8);

  xi = _mm512_ipcvt_roundps_epi32 (x, 4);
  xi = _mm512_mask_ipcvt_roundps_epi32 (xi, m16, x, 8);
  xi = _mm512_maskz_ipcvt_roundps_epi32 (m16, x, 11);

  xi = _mm512_ipcvt_roundps_epu32 (x, 4);
  xi = _mm512_mask_ipcvt_roundps_epu32 (xi, m16, x, 8);
  xi = _mm512_maskz_ipcvt_roundps_epu32 (m16, x, 11);

  xi = _mm512_ipcvtt_roundps_epi32 (x, 4);
  xi = _mm512_mask_ipcvtt_roundps_epi32 (xi, m16, x, 8);
  xi = _mm512_maskz_ipcvtt_roundps_epi32 (m16, x, 8);

  xi = _mm512_ipcvtt_roundps_epu32 (x, 4);
  xi = _mm512_mask_ipcvtt_roundps_epu32 (xi, m16, x, 8);
  xi = _mm512_maskz_ipcvtt_roundps_epu32 (m16, x, 8);

  xi = _mm512_ipcvtnebf16_epi16 (xbh);
  xi = _mm512_mask_ipcvtnebf16_epi16 (xi, m32, xbh);
  xi = _mm512_maskz_ipcvtnebf16_epi16 (m32, xbh);

  xi = _mm512_ipcvtnebf16_epu16 (xbh);
  xi = _mm512_mask_ipcvtnebf16_epu16 (xi, m32, xbh);
  xi = _mm512_maskz_ipcvtnebf16_epu16 (m32, xbh);

  xi = _mm512_ipcvttnebf16_epi16 (xbh);
  xi = _mm512_mask_ipcvttnebf16_epi16 (xi, m32, xbh);
  xi = _mm512_maskz_ipcvttnebf16_epi16 (m32, xbh);

  xi = _mm512_ipcvttnebf16_epu16 (xbh);
  xi = _mm512_mask_ipcvttnebf16_epu16 (xi, m32, xbh);
  xi = _mm512_maskz_ipcvttnebf16_epu16 (m32, xbh);

  hxi = _mm512_cvtts_roundpd_epi32 (xd, 8);
  hxi = _mm512_mask_cvtts_roundpd_epi32 (hxi, m8, xd, 8);
  hxi = _mm512_maskz_cvtts_roundpd_epi32 (m8, xd, 8);

  xi = _mm512_cvtts_roundpd_epi64 (xd, 8);
  xi = _mm512_mask_cvtts_roundpd_epi64 (xi, m8, xd, 8);
  xi = _mm512_maskz_cvtts_roundpd_epi64 (m8, xd, 8);

  hxi = _mm512_cvtts_roundpd_epu32 (xd, 8);
  hxi = _mm512_mask_cvtts_roundpd_epu32 (hxi, m8, xd, 8);
  hxi = _mm512_maskz_cvtts_roundpd_epu32 (m8, xd, 8);

  xi = _mm512_cvtts_roundpd_epu64 (xd, 8);
  xi = _mm512_mask_cvtts_roundpd_epu64 (xi, m8, xd, 8);
  xi = _mm512_maskz_cvtts_roundpd_epu64 (m8, xd, 8);

  xi = _mm512_cvtts_roundps_epi32 (x, 8);
  xi = _mm512_mask_cvtts_roundps_epi32 (xi, m16, x, 8);
  xi = _mm512_maskz_cvtts_roundps_epi32 (m16, x, 8);

  xi = _mm512_cvtts_roundps_epi64 (hx, 8);
  xi = _mm512_mask_cvtts_roundps_epi64 (xi, m8, hx, 8);
  xi = _mm512_maskz_cvtts_roundps_epi64 (m8, hx, 8);

  xi = _mm512_cvtts_roundps_epu32 (x, 8);
  xi = _mm512_mask_cvtts_roundps_epu32 (xi, m16, x, 8);
  xi = _mm512_maskz_cvtts_roundps_epu32 (m16, x, 8);

  xi = _mm512_cvtts_roundps_epu64 (hx, 8);
  xi = _mm512_mask_cvtts_roundps_epu64 (xi, m8, hx, 8);
  xi = _mm512_maskz_cvtts_roundps_epu64 (m8, hx, 8);
}
