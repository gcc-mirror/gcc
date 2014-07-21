/* { dg-do compile } */
/* { dg-options "-mavx512f" } */

#include <x86intrin.h>

__m512 test (__m512 x)
{
  return _mm512_getmant_ps(x, _MM_MANT_NORM_1_2, _MM_MANT_SIGN_zero);
}

