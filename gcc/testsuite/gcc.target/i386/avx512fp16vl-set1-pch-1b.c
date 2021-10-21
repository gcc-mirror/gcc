/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl" } */

#include<stdio.h>
#include <math.h>
#include <complex.h>

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16

#include <immintrin.h>
#include "avx512-check.h"

static void
do_test (void)
{
  _Float16 _Complex fc = 1.0 + 1.0*I;
  union
  { 
    _Float16 _Complex a;
    float b;
  } u = { .a = fc };
  float ff= u.b;

  typedef union
  {
    float fp[8];
    __m256h m256h;
  } u1;

  __m256h test256 = _mm256_set1_pch(fc);

  u1 test1;
  test1.m256h = test256;
  for (int i = 0; i<8; i++)
  {
    if (test1.fp[i] != ff) abort();
  }

  typedef union
  {
    float fp[4];
    __m128h m128h;
  } u2;

  __m128h test128 = _mm_set1_pch(fc);

  u2 test2;
  test2.m128h = test128;
  for (int i = 0; i<4; i++)
  {
    if (test2.fp[i] != ff) abort();
  }

}
