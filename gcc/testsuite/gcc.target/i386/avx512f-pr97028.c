/* PR target/97028 */
/* { dg-do assemble { target avx512f } } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-O2 -mavx512f -masm=intel" } */

#include <x86intrin.h>

__m512
foo (__m512 x, float *y)
{
  return _mm512_mul_ps (x, _mm512_set1_ps (*y));
}

__m512
bar (__m512 x, float *y)
{
  return _mm512_div_ps (x, _mm512_set1_ps (*y));
}
