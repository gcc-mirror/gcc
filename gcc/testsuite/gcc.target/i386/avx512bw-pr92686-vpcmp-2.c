/* { dg-do run } */
/* { dg-require-effective-target avx512bw } */
/* { dg-options "-O2 -mavx512bw" } */

#ifndef CHECK
#define CHECK "avx512f-helper.h"
#endif

#include CHECK

#ifndef TEST
#define TEST avx512bw_test
#endif

#include "avx512bw-pr92686-vpcmp-1.c"

#define NUM 512

#define TEST_SIGNED(vtype, type, N, fn, op) \
do								\
  {								\
    union { vtype x[NUM / N]; type i[NUM]; } dst, src1, src2;	\
    int i, sign = 1;						\
    type res;							\
    for (i = 0; i < NUM; i++)					\
      {								\
	src1.i[i] = i * i * sign;				\
	src2.i[i] = (i + 20) * sign;				\
	sign = -sign;						\
      }								\
    for (i = 0; i < NUM; i += N)				\
      dst.x[i / N] = fn (src1.x[i / N], src2.x[i / N]);		\
								\
    for (i = 0; i < NUM; i++)					\
      {								\
	res = src1.i[i] op src2.i[i] ? -1 : 0;			\
	if (res != dst.i[i])					\
	  abort ();						\
      }								\
  }								\
while (0)

#define TEST_UNSIGNED(vtype, type, N, fn, op) \
do								\
  {								\
    union { vtype x[NUM / N]; type i[NUM]; } dst, src1, src2;	\
    int i;							\
    type res;							\
								\
    for (i = 0; i < NUM; i++)					\
      {								\
	src1.i[i] = i * i;					\
	src2.i[i] = i + 20;					\
	if ((i % 4))						\
	  src2.i[i] |= (1ULL << (sizeof (type)			\
				 * __CHAR_BIT__ - 1));		\
      }								\
								\
    for (i = 0; i < NUM; i += N)				\
      dst.x[i / N] = fn (src1.x[i / N], src2.x[i / N]);		\
								\
    for (i = 0; i < NUM; i++)					\
      {								\
	res = src1.i[i] op src2.i[i] ? -1 : 0;			\
	if (res != dst.i[i])					\
	  abort ();						\
      }								\
  }								\
while (0)

static void
TEST (void)
{
  TEST_SIGNED (v64qi, signed char, 64, f1, >=);
  TEST_UNSIGNED (v64uqi, unsigned char, 64, f2, >=);
  TEST_SIGNED (v64qi, signed char, 64, f3, <=);
  TEST_UNSIGNED (v64uqi, unsigned char, 64, f4, <=);
  TEST_SIGNED (v32hi, short int, 32, f5, >=);
  TEST_UNSIGNED (v32uhi, unsigned short int, 32, f6, >=);
  TEST_SIGNED (v32hi, short int, 32, f7, <=);
  TEST_UNSIGNED (v32uhi, unsigned short int, 32, f8, <=);
  TEST_SIGNED (v16si, int, 16, f9, >=);
  TEST_UNSIGNED (v16usi, unsigned int, 16, f10, >=);
  TEST_SIGNED (v16si, int, 16, f11, <=);
  TEST_UNSIGNED (v16usi, unsigned int, 16, f12, <=);
  TEST_SIGNED (v8di, long long int, 8, f13, >=);
  TEST_UNSIGNED (v8udi, unsigned long long int, 8, f14, >=);
  TEST_SIGNED (v8di, long long int, 8, f15, <=);
  TEST_UNSIGNED (v8udi, unsigned long long int, 8, f16, <=);
}
