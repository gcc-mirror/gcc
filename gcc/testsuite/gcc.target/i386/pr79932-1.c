/* PR target/79932 */
/* { dg-do compile } */
/* { dg-options "-O0 -mavx512bw" } */

#include <x86intrin.h>

__m512i a, b, c, d, e, f, g, h, i;
__mmask32 m;

void
foo (void)
{
  d = _mm512_packs_epi32 (a, b);
  e = _mm512_maskz_packs_epi32 (m, a, b);
  f = _mm512_mask_packs_epi32 (c, m, a, b);
  g = _mm512_packus_epi32 (a, b);
  h = _mm512_maskz_packus_epi32 (m, a, b);
  i = _mm512_mask_packus_epi32 (c, m, a, b);
}
