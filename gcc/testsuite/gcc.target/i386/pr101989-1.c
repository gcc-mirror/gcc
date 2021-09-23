/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "vpternlog" 6 } } */
/* { dg-final { scan-assembler-not "vpxor" } } */
/* { dg-final { scan-assembler-not "vpor" } } */
/* { dg-final { scan-assembler-not "vpand" } } */

#include<immintrin.h>
__m256d
__attribute__((noipa, target("avx512vl")))
copysign2_pd(__m256d from, __m256d to) {
  __m256i a = _mm256_castpd_si256(from);
  __m256d avx_signbit = _mm256_castsi256_pd(_mm256_slli_epi64(_mm256_cmpeq_epi64(a, a), 63));
  /* (avx_signbit & from) | (~avx_signbit & to)  */
  return _mm256_or_pd(_mm256_and_pd(avx_signbit, from), _mm256_andnot_pd(avx_signbit, to));
}

__m256i
__attribute__((noipa, target("avx512vl")))
foo (__m256i src1, __m256i src2, __m256i src3)
{
  return (src2 & ~src1) | (src3 & src1);
}

__m256i
__attribute__ ((noipa, target("avx512vl")))
foo1 (__m256i src1, __m256i src2, __m256i src3)
{
  return (src2 & src1) | (src3 & ~src1);
}

__m256i
__attribute__ ((noipa, target("avx512vl")))
foo2 (__m256i src1, __m256i src2, __m256i src3)
{
  return (src2 & src1) | (~src3 & src1);
}

__m256i
__attribute__ ((noipa, target("avx512vl")))
foo3 (__m256i src1, __m256i src2, __m256i src3)
{
  return (~src2 & src1) | (src3 & src1);
}

__m256i
__attribute__ ((noipa, target("avx512vl")))
foo4 (__m256i src1, __m256i src2, __m256i src3)
{
  return src3 & src2 ^ src1;
}
