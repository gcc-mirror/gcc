/* { dg-do compile } */
/* { dg-options "-mavx -O2 -mno-avx2" } */
/* { dg-final { scan-assembler-times {vblendvps[ \t]+%ymm[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vblendvpd[ \t]+%ymm[0-9]+} 1 } } */

#include <immintrin.h>

__m256 bend_stuff( __m256 a, __m256 b, __m256 mask)
{
  return _mm256_blendv_ps(a, b, mask);
}

__m256d bend_stuff1( __m256d a, __m256d b, __m256d mask)
{
  return _mm256_blendv_pd(a, b, mask);
}
