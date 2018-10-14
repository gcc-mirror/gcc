/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-march=corei7 -O2" } */
/* { dg-final { scan-assembler-times "punpcklqdq\[ \\t\]+%xmm\[0-9\]+,\[ \\t\]+%xmm\[0-9\]+" 1 } } */

#include <immintrin.h>

__m128i
foo (long long val)
{
  __m128i rval = {val, val};
  return rval;
}
