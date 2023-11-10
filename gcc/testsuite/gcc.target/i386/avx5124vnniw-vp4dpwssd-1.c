/* { dg-do compile } */
/* { dg-options "-O2 -mavx5124vnniw" } */
/* { dg-warning "AVX5124VNNIW support will be removed in GCC 15" "" { target *-*-* } 0 } */
/* { dg-final { scan-assembler-times "vp4dpwssd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vp4dpwssd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vp4dpwssd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <x86intrin.h>

__m512i a, b, c, d, e, f, g, x1, x2, x3;
__m128i *mem;
__mmask16 m;

int foo ()
{
  x1 = _mm512_4dpwssd_epi32 (a, b, c, d, e, mem);
  x2 = _mm512_mask_4dpwssd_epi32 (a, m, b, c, d, e, mem);
  x3 = _mm512_maskz_4dpwssd_epi32 (m, a, b, c, d, e, mem);
}
