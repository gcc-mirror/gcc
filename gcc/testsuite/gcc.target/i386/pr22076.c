/* { dg-do compile } */
/* { dg-options "-O2 -flax-vector-conversions -mmmx" } */

#include <mmintrin.h>

__v8qi test ()
{
  __v8qi mm0 = {1,2,3,4,5,6,7,8};
  __v8qi mm1 = {11,22,33,44,55,66,77,88};
  volatile __m64 x;

  x = _mm_add_pi8 (mm0, mm1);

  return x;
}

/* { dg-final { scan-assembler-times "movq" 3 } } */
/* { dg-final { scan-assembler-times "movl" 1 { target { ilp32 && nonpic } } } } */
/* { dg-final { scan-assembler-times "movl" 2 { target { ilp32 && { ! nonpic } } } } } */
