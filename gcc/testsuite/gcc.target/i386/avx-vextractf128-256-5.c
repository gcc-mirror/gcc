/* { dg-do assemble { target { ! ia32 } } } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O2 -mavx512f" } */

#include <immintrin.h>

register __m512d z asm ("zmm16"); /* { dg-warning "call-clobbered register used for global register variable" } */

__m128d foo ()
{
  return _mm256_extractf128_pd (_mm512_extractf64x4_pd(z, 0), 1);
}
