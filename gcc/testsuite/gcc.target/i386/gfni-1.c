/* { dg-do compile } */
/* { dg-options "-mgfni -mavx512bw -mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vgf2p8affineinvqb\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgf2p8affineinvqb\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgf2p8affineinvqb\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgf2p8affineqb\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgf2p8affineqb\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgf2p8affineqb\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgf2p8mulb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgf2p8mulb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgf2p8mulb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <x86intrin.h>

volatile __m512i x1, x2;
volatile __mmask64 m64;
 
void extern
avx512vl_test (void)
{
    x1 = _mm512_gf2p8affineinv_epi64_epi8(x1, x2, 3);
    x1 = _mm512_mask_gf2p8affineinv_epi64_epi8(x1, m64, x2, x1, 3);
    x1 = _mm512_maskz_gf2p8affineinv_epi64_epi8(m64, x1, x2, 3);
    x1 = _mm512_gf2p8affine_epi64_epi8(x1, x2, 3);
    x1 = _mm512_mask_gf2p8affine_epi64_epi8(x1, m64, x2, x1, 3);
    x1 = _mm512_maskz_gf2p8affine_epi64_epi8(m64, x1, x2, 3);
    x1 = _mm512_gf2p8mul_epi8(x1, x2);
    x1 = _mm512_mask_gf2p8mul_epi8(x1, m64, x2, x1);
    x1 = _mm512_maskz_gf2p8mul_epi8(m64, x1, x2);
}
