/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-skip-if "" { i?86-*-* x86_64-*-* } { "-march=*" } { "-march=core2" } } */
/* { dg-options "-O2 -msse4 -march=core2 -dp" } */

#include <emmintrin.h>

__m128i
test (long long b)
{
  return _mm_cvtsi64_si128 (b); 
}

/* { dg-final { scan-assembler-times "vec_concatv2di/3" 1 } } */
