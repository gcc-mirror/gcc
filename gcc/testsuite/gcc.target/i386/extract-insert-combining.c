/* { dg-do compile } */
/* { dg-options "-msse4.2 -O3" } */
/* { dg-final { scan-assembler-times "(?:vmovd|movd)\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]" 3 } } */
/* { dg-final { scan-assembler-times "(?:vpaddd|paddd)\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "(?:vpinsrd|pinsrd)\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-not "vmovss" } } */
/* { dg-final { scan-assembler-not {(?n)mov.*(%rsp)} { target { ! ia32 } } } } */

#include <immintrin.h>

int
main (int a, int b)
{
  int res;

  __m128i xa, xb, xres;

  xa = _mm_insert_epi32 (xa, a, 0);
  xb = _mm_insert_epi32 (xb, b, 0);

  xres = _mm_add_epi32 (xa, xb);

  res = _mm_extract_epi32 (xres, 0);

  xres = _mm_insert_epi32 (xres, res, 0);
  xb   = _mm_insert_epi32 (xb, b, 0);

  xres = _mm_add_epi32 (xres, xb);

  res = _mm_extract_epi32 (xres, 0);

  return res;
}

