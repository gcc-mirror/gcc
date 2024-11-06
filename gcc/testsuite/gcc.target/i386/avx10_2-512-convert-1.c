/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2-512 -O2" } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2bf8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2bf8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2bf8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2bf8s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2bf8s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2bf8s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2hf8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2hf8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2hf8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2hf8s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2hf8s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2hf8s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2bf8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2bf8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2bf8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2bf8s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2bf8s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2bf8s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2hf8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2hf8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2hf8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2hf8s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2hf8s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2hf8s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ph\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ph\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ph\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2bf8\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2bf8\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2bf8\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2bf8s\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2bf8s\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2bf8s\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2hf8\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2hf8\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2hf8\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2hf8s\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2hf8s\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2hf8s\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpsllw\[ \t]\+\\\$8, %zmm\[0-9]\+, %zmm\[0-9]\+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpsllw\[ \t]\+\\\$8, %zmm\[0-9]\+, %zmm\[0-9]\+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovsxbw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%zmm\[0-9\](?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpmovsxbw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256i x256i, z1;
volatile __m512i x512i;
volatile __m512 x, a1, b1;
volatile __m512h y, x512h, z;
volatile __mmask16 m16;
volatile __mmask32 m32;
volatile __mmask64 m64;
const void *a;
__m512bh *c;
__m512h *d;

void extern
avx10_2_512_test (void)
{ 
  y = _mm512_cvtx2ps_ph (a1, b1);
  y = _mm512_mask_cvtx2ps_ph (y, m32, a1, b1);
  y = _mm512_maskz_cvtx2ps_ph (m32, a1, b1);

  y = _mm512_cvtx_round2ps_ph (a1, b1, 8);
  y = _mm512_mask_cvtx_round2ps_ph (y, m32, a1, b1, 8);
  y = _mm512_maskz_cvtx_round2ps_ph (m32, a1, b1, 8);
}

void extern
avx10_2_512_vcvtbiasph2bf8_test (void)
{
  x256i = _mm512_cvtbiasph_pbf8 (x512i, x512h);
  x256i = _mm512_mask_cvtbiasph_pbf8 (x256i, m32, x512i, x512h);
  x256i = _mm512_maskz_cvtbiasph_pbf8 (m32, x512i, x512h);
}

void extern
avx10_2_512_vcvtbiasph2bf8s_test (void)
{
  x256i = _mm512_cvtbiassph_pbf8 (x512i, x512h);
  x256i = _mm512_mask_cvtbiassph_pbf8 (x256i, m32, x512i, x512h);
  x256i = _mm512_maskz_cvtbiassph_pbf8 (m32, x512i, x512h);
}

void extern
avx10_2_512_vcvtbiasph2hf8_test (void)
{
  x256i = _mm512_cvtbiasph_phf8 (x512i, x512h);
  x256i = _mm512_mask_cvtbiasph_phf8 (x256i, m32, x512i, x512h);
  x256i = _mm512_maskz_cvtbiasph_phf8 (m32, x512i, x512h);
}

void extern
avx10_2_512_vcvtbiasph2hf8s_test (void)
{
  x256i = _mm512_cvtbiassph_phf8 (x512i, x512h);
  x256i = _mm512_mask_cvtbiassph_phf8 (x256i, m32, x512i, x512h);
  x256i = _mm512_maskz_cvtbiassph_phf8 (m32, x512i, x512h);
}

void extern
avx10_2_512_vcvtne2ph2bf8_test (void)
{
  x512i = _mm512_cvtne2ph_pbf8 (x512h, x512h);
  x512i = _mm512_mask_cvtne2ph_pbf8 (x512i, m64, x512h, x512h);
  x512i = _mm512_maskz_cvtne2ph_pbf8 (m64, x512h, x512h);
}

void extern
avx10_2_512_vcvtne2ph2bf8s_test (void)
{
  x512i = _mm512_cvtnes2ph_pbf8 (x512h, x512h);
  x512i = _mm512_mask_cvtnes2ph_pbf8 (x512i, m64, x512h, x512h);
  x512i = _mm512_maskz_cvtnes2ph_pbf8 (m64, x512h, x512h);
}

void extern
avx10_2_512_vcvtne2ph2hf8_test (void)
{
  x512i = _mm512_cvtne2ph_phf8 (x512h, x512h);
  x512i = _mm512_mask_cvtne2ph_phf8 (x512i, m64, x512h, x512h);
  x512i = _mm512_maskz_cvtne2ph_phf8 (m64, x512h, x512h);
}

void extern
avx10_2_512_vcvtne2ph2hf8s_test (void)
{
  x512i = _mm512_cvtnes2ph_phf8 (x512h, x512h);
  x512i = _mm512_mask_cvtnes2ph_phf8 (x512i, m64, x512h, x512h);
  x512i = _mm512_maskz_cvtnes2ph_phf8 (m64, x512h, x512h);
}

void extern
avx10_2_512_vcvthf82ph_test (void)
{
  x512h = _mm512_cvthf8_ph (x256i);
  x512h = _mm512_mask_cvthf8_ph (x512h, m32, x256i);
  x512h = _mm512_maskz_cvthf8_ph (m32, x256i);
}

void extern
avx10_2_512_vcvtneph2bf8_test (void)
{
  x256i = _mm512_cvtneph_pbf8 (x512h);
  x256i = _mm512_mask_cvtneph_pbf8 (x256i, m32, x512h);
  x256i = _mm512_maskz_cvtneph_pbf8 (m32, x512h);
}

void extern
avx10_2_512_vcvtneph2bf8s_test (void)
{
  x256i = _mm512_cvtnesph_pbf8 (x512h);
  x256i = _mm512_mask_cvtnesph_pbf8 (x256i, m32, x512h);
  x256i = _mm512_maskz_cvtnesph_pbf8 (m32, x512h);
}

void extern
avx10_2_512_vcvtneph2hf8_test (void)
{
  x256i = _mm512_cvtneph_phf8 (x512h);
  x256i = _mm512_mask_cvtneph_phf8 (x256i, m32, x512h);
  x256i = _mm512_maskz_cvtneph_phf8 (m32, x512h);
}

void extern
avx10_2_512_vcvtneph2hf8s_test (void)
{
  x256i = _mm512_cvtnesph_phf8 (x512h);
  x256i = _mm512_mask_cvtnesph_phf8 (x256i, m32, x512h);
  x256i = _mm512_maskz_cvtnesph_phf8 (m32, x512h);
}

void extern
avx10_2_512_cvtbf8_fp16_test (void)
{
  y = _mm512_cvtpbf8_ph (z1);
  y = _mm512_mask_cvtpbf8_ph (z, m16, z1);
  y = _mm512_maskz_cvtpbf8_ph (m16, z1);
}
