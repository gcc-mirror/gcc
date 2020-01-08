/* { dg-do run } */
/* { dg-require-effective-target avx512bw } */
/* { dg-require-effective-target avx512vl } */
/* { dg-options "-Ofast -mavx512bw -mavx512vl -mprefer-vector-width=256" } */

#ifndef CHECK
#define CHECK "avx512f-helper.h"
#endif

#include CHECK

#ifndef TEST
#define TEST avx512bw_test
#endif

#include "avx512bw-pr92686-movcc-1.c"
#include "pr92686.inc"

#define NUM 512


#define TEST_SIGNED(vtype, type, N, fn, fn2, op)		\
do								\
  {								\
    type dst[NUM], src1[NUM], src2[NUM];			\
    int i, j,  sign = 1;					\
    type res[N];						\
    for (i = 0; i < NUM; i++)					\
      {								\
	src1[i] = i * i * sign;					\
	src2[i] = (i + 20) * sign;				\
	dst[i] = i * i * i + 100;				\
	sign = -sign;						\
      }								\
    for (i = 0; i < NUM; i += N)				\
      {								\
	for (j = 0; j < N; j++)					\
	  res[j] = dst[i + j];					\
	fn (&dst[i], &src1[i], &src2[i]);			\
	for (j = 0; j < N; j++)					\
	  {							\
	    res[j] = fn2 (res[j], src1[i + j],			\
			  src2[i+ j], op);			\
	    if (res[j] != dst[i+ j])				\
	      abort();						\
	  }							\
      }								\
  }								\
while (0)

#define TEST_UNSIGNED(vtype, type, N, fn, fn2, op)		\
do								\
  {								\
    type dst[NUM], src1[NUM], src2[NUM];			\
    int i,j;							\
    type res[N];						\
								\
    for (i = 0; i < NUM; i++)					\
      {								\
	src1[i] = i * i;					\
	src2[i] = i + 20;					\
	dst[i] = i * i * i + 100;				\
	if ((i % 4))						\
	  src2[i] |= (1ULL << (sizeof (type)			\
				 * __CHAR_BIT__ - 1));		\
      }								\
    for (i = 0; i < NUM; i += N)				\
      {								\
	for (j = 0; j < N; j++)					\
	  res[j] = dst[i + j];					\
	fn (&dst[i], &src1[i], &src2[i]);			\
	for (j = 0; j < N; j++)					\
	  {							\
	    res[j] = fn2 (res[j], src1[i + j],			\
			  src2[i + j], op);			\
	    if (res[j] != dst[i + j])				\
	      abort();						\
	  }							\
      }								\
  }								\
while (0)

static void
TEST (void)
{
  TEST_SIGNED (v64qi, signed char, 64, f1, cmpb, 5);
  TEST_UNSIGNED (v64uqi, unsigned char, 64, f2, cmpub, 5);
  TEST_SIGNED (v64qi, signed char, 64, f3, cmpb, 2);
  TEST_UNSIGNED (v64uqi, unsigned char, 64, f4, cmpub, 2);
  TEST_SIGNED (v32hi, short int, 32, f5, cmpw, 5);
  TEST_UNSIGNED (v32uhi, unsigned short int, 32, f6, cmpuw, 5);
  TEST_SIGNED (v32hi, short int, 32, f7, cmpw, 2);
  TEST_UNSIGNED (v32uhi, unsigned short int, 32, f8, cmpuw, 2);
  TEST_SIGNED (v16si, int, 16, f9, cmpd, 5);
  TEST_UNSIGNED (v16usi, unsigned int, 16, f10, cmpud, 5);
  TEST_SIGNED (v16si, int, 16, f11, cmpd, 2);
  TEST_UNSIGNED (v16usi, unsigned int, 16, f12, cmpud, 2);
  TEST_SIGNED (v8di, long long int, 8, f13, cmpq, 5);
  TEST_UNSIGNED (v8udi, unsigned long long int, 8, f14, cmpuq, 5);
  TEST_SIGNED (v8di, long long int, 8, f15, cmpq, 2);
  TEST_UNSIGNED (v8udi, unsigned long long int, 8, f16, cmpuq, 2);
}
