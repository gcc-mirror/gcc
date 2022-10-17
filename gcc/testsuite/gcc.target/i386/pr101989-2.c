/* { dg-do run } */
/* { dg-options "-O2 -mavx2 -mno-avx512f" } */
/* { dg-require-effective-target avx512vl } */

#define AVX512VL

#include "avx512f-helper.h"

#include "pr101989-1.c"
__m256d
avx2_copysign2_pd (__m256d from, __m256d to) {
  __m256i a = _mm256_castpd_si256(from);
  __m256d avx_signbit = _mm256_castsi256_pd(_mm256_slli_epi64(_mm256_cmpeq_epi64(a, a), 63));
  /* (avx_signbit & from) | (~avx_signbit & to)  */
  return _mm256_or_pd(_mm256_and_pd(avx_signbit, from), _mm256_andnot_pd(avx_signbit, to));
}

__m256i
avx2_foo (__m256i src1, __m256i src2, __m256i src3)
{
  return (src2 & ~src1) | (src3 & src1);
}

__m256i
avx2_foo1 (__m256i src1, __m256i src2, __m256i src3)
{
  return (src2 & src1) | (src3 & ~src1);
}

__m256i
avx2_foo2 (__m256i src1, __m256i src2, __m256i src3)
{
  return (src2 & src1) | (~src3 & src1);
}

__m256i
avx2_foo3 (__m256i src1, __m256i src2, __m256i src3)
{
  return (~src2 & src1) | (src3 & src1);
}

__m256i
avx2_foo4 (__m256i src1, __m256i src2, __m256i src3)
{
  return src3 & src2 ^ src1;
}


void
test_256 (void)
{
  union256i_q q1, q2, q3, res2, exp2;
  union256d d1, d2, res1, exp1;
  int i, sign = 1;

  for (i = 0; i < 4; i++)
    {
      d1.a[i] = 12.34 * (i + 2000) * sign;
      d2.a[i] = 56.78 * (i - 30) * sign;
      q1.a[i] = 12 * (i + 2000) * sign;
      q2.a[i] = 56 * (i - 30) * sign;
      q3.a[i] = 90 * (i + 40) * sign;
      res1.a[i] = DEFAULT_VALUE;
      exp1.a[i] = DEFAULT_VALUE;
      res2.a[i] = exp2.a[i] = -1;
      sign = -sign;
    }

  exp1.x = avx2_copysign2_pd (d1.x, d2.x);
  res1.x = copysign2_pd (d1.x, d2.x);
  if (UNION_CHECK (256, d) (res1, exp1.a))
    abort ();

  exp2.x = avx2_foo1 (q1.x, q2.x, q3.x);
  res2.x = foo1 (q1.x, q2.x, q3.x);
  if (UNION_CHECK (256, i_q) (res2, exp2.a))
    abort ();

  exp2.x = avx2_foo2 (q1.x, q2.x, q3.x);
  res2.x = foo2 (q1.x, q2.x, q3.x);
  if (UNION_CHECK (256, i_q) (res2, exp2.a))
    abort ();

  exp2.x = avx2_foo3 (q1.x, q2.x, q3.x);
  res2.x = foo3 (q1.x, q2.x, q3.x);
  if (UNION_CHECK (256, i_q) (res2, exp2.a))
    abort ();

  exp2.x = avx2_foo4 (q1.x, q2.x, q3.x);
  res2.x = foo4 (q1.x, q2.x, q3.x);
  if (UNION_CHECK (256, i_q) (res2, exp2.a))
    abort ();

  exp2.x = avx2_foo (q1.x, q2.x, q3.x);
  res2.x = foo (q1.x, q2.x, q3.x);
  if (UNION_CHECK (256, i_q) (res2, exp2.a))
    abort ();
}

static void
test_128 ()
{}
