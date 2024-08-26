/* { dg-do compile } */
/* { dg-options "-O2 -mavx10.2 -mavx10.2-512" } */
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

#include <immintrin.h>

volatile __m512 x;
volatile __m512h xh;
volatile __m512i xi;
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
}
