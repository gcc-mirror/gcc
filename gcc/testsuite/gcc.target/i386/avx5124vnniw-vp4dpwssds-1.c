/* { dg-do compile } */
/* { dg-options "-O2 -mavx5124vnniw" } */
/* { dg-final { scan-assembler-times "vp4dpwssds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vp4dpwssds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vp4dpwssds\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <x86intrin.h>

__m512i a, b, c, d, e, f, g, x1, x2, x3;
__m128i *mem;
__mmask16 m;

int foo ()
{
  x1 = _mm512_4dpwssds_epi32 (a, b, c, d, e, mem);
  x2 = _mm512_mask_4dpwssds_epi32 (a, m, b, c, d, e, mem);
  x3 = _mm512_maskz_4dpwssds_epi32 (m, a, b, c, d, e, mem);
}
