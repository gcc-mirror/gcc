/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16" } */

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
    float fp[16];
    __m512h m512h;
  } u1;

  __m512h test512 = _mm512_set1_pch(fc);

  u1 test;
  test.m512h = test512;
  for (int i = 0; i<16; i++)
  {
    if (test.fp[i] != ff) abort();
  }

}
