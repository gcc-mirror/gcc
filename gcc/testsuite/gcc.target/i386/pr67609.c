/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target lp64 } */
/* { dg-final { scan-assembler "movdqa" } } */

#include <emmintrin.h>
__m128d reg;
void set_lower(double b)
{
  double v[2];
  _mm_store_pd(v, reg);
  v[0] = b;
  reg = _mm_load_pd(v);
}
