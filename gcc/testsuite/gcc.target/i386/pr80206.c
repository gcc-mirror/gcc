/* PR target/80206 */
/* { dg-do compile } */
/* { dg-options "-mavx512f -ffloat-store" } */

#include <immintrin.h>

__m512d a;
__m256d b;

void
foo (__m256d *p)
{
  *p = _mm512_mask_extractf64x4_pd (b, 1, a, 1);
}
