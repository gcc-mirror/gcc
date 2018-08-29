/* PR rtl-optimization/84989 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */

#include <x86intrin.h>

__m512
foo (float a, float *b)
{
  return _mm512_sub_ps (_mm512_broadcast_f32x4 (_mm_load_ps (b)),
			_mm512_broadcast_f32x4 (_mm_set1_ps (a)));
}
