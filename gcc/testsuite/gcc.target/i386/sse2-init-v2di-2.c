/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -msse4 -march=core2 -dp" } */

#include <emmintrin.h>

__m128i
test (long long b)
{
  return _mm_cvtsi64_si128 (b); 
}

/* { dg-final { scan-assembler-times "\\*vec_concatv2di_rex64_sse4_1/3" 1 } } */
