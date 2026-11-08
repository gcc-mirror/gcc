/* { dg-do compile } */
/* { dg-options "-O2 -mavx512dq -mno-avx512vl" } */
/* { dg-final { scan-assembler "vinserti128" } } */
/* { dg-final { scan-assembler-not "vinserti64x2" } } */

#include <immintrin.h>
__m256i
f4 (long long a, long long b, long long c, long long d)
{
  return _mm256_set_epi64x (a, b, c, d);
}
