/* { dg-do compile } */
/* { dg-options "-O2 -mavx5124fmaps" } */
/* { dg-final { scan-assembler-times "v4fnmaddps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "v4fnmaddps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "v4fnmaddps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <x86intrin.h>

__m512 a, b, c, d, e, f, g, x1, x2, x3;
__m128 *mem;
__mmask16 m;

int foo ()
{
  x1 = _mm512_4fnmadd_ps (a, b, c, d, e, mem);
  x2 = _mm512_mask_4fnmadd_ps (a, m, b, c, d, e, mem);
  x3 = _mm512_maskz_4fnmadd_ps (m, a, b, c, d, e, mem);
}
