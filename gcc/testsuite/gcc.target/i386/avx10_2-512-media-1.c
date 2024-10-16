/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2-512 -O2" } */
/* { dg-final { scan-assembler-times "vpdpbssd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbssd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbssd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbssds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbssds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbssds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbsud\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbsud\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbsud\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbsuds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbsuds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbsuds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuud\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuud\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuud\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuuds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuuds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuuds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwsud\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwsud\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwsud\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwsuds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwsuds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwsuds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuud\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuud\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuud\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuuds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuuds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuuds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vdpphps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vdpphps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vdpphps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmpsadbw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmpsadbw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmpsadbw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */


#include <immintrin.h>

volatile __m512 a;
volatile __m512h b,c;
volatile __m512i x,y,z,z1;
volatile __mmask16 m16;
volatile __mmask32 m32;

void avx10_2_512_test (void)
{
  x = _mm512_dpbssd_epi32 (x, y, z);
  x = _mm512_mask_dpbssd_epi32 (x, m16, y, z);
  x = _mm512_maskz_dpbssd_epi32 (m16, x, y, z);

  x = _mm512_dpbssds_epi32 (x, y, z);
  x = _mm512_mask_dpbssds_epi32 (x, m16, y, z);
  x = _mm512_maskz_dpbssds_epi32 (m16, x, y, z);

  x = _mm512_dpbsud_epi32 (x, y, z);
  x = _mm512_mask_dpbsud_epi32 (x, m16, y, z);
  x = _mm512_maskz_dpbsud_epi32 (m16, x, y, z);

  x = _mm512_dpbsuds_epi32 (x, y, z);
  x = _mm512_mask_dpbsuds_epi32 (x, m16, y, z);
  x = _mm512_maskz_dpbsuds_epi32 (m16, x, y, z);

  x = _mm512_dpbuud_epi32 (x, y, z);
  x = _mm512_mask_dpbuud_epi32 (x, m16, y, z);
  x = _mm512_maskz_dpbuud_epi32 (m16, x, y, z);

  x = _mm512_dpbuuds_epi32 (x, y, z);
  x = _mm512_mask_dpbuuds_epi32 (x, m16, y, z);
  x = _mm512_maskz_dpbuuds_epi32 (m16, x, y, z);

  x = _mm512_dpwsud_epi32 (x, y, z);
  x = _mm512_mask_dpwsud_epi32 (x, m16, y, z);
  x = _mm512_maskz_dpwsud_epi32 (m16, x, y, z);

  x = _mm512_dpwsuds_epi32 (x, y, z);
  x = _mm512_mask_dpwsuds_epi32 (x, m16, y, z);
  x = _mm512_maskz_dpwsuds_epi32 (m16, x, y, z);

  x = _mm512_dpwusd_epi32 (x, y, z);
  x = _mm512_mask_dpwusd_epi32 (x, m16, y, z);
  x = _mm512_maskz_dpwusd_epi32 (m16, x, y, z);

  x = _mm512_dpwusds_epi32 (x, y, z);
  x = _mm512_mask_dpwusds_epi32 (x, m16, y, z);
  x = _mm512_maskz_dpwusds_epi32 (m16, x, y, z);

  x = _mm512_dpwuud_epi32 (x, y, z);
  x = _mm512_mask_dpwuud_epi32 (x, m16, y, z);
  x = _mm512_maskz_dpwuud_epi32 (m16, x, y, z);

  x = _mm512_dpwuuds_epi32 (x, y, z);
  x = _mm512_mask_dpwuuds_epi32 (x, m16, y, z);
  x = _mm512_maskz_dpwuuds_epi32 (m16, x, y, z);

  a = _mm512_dpph_ps (a, b, c);
  a = _mm512_mask_dpph_ps (a, m16, b, c);
  a = _mm512_maskz_dpph_ps (m16, a, b, c);

  x = _mm512_mpsadbw_epu8 (x, y, 1);
  x = _mm512_mask_mpsadbw_epu8 (x, m32, y, z, 1);
  x = _mm512_maskz_mpsadbw_epu8 (m32, x, y, 1);
}
