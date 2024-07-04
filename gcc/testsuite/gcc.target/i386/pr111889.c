/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64" } */

#include <immintrin.h>

__attribute__ ((target ("no-evex512,avx512vl")))
__m256d foo (__m256d __W, __mmask8 __U, __m256d __A)
{
  return _mm256_mask_mov_pd (__W, __U, __A);
}
