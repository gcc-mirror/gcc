/* { dg-do run } */
/* { dg-require-effective-target sse2 } */
/* { dg-options "-O2 -msse2" } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

#include "sse2-pr88547-1.c"

#define NUM 256

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
  TEST_SIGNED (v16qi, signed char, 16, f1, <=);
  TEST_UNSIGNED (v16uqi, unsigned char, 16, f2, <=);
  TEST_SIGNED (v16qi, signed char, 16, f3, >=);
  TEST_UNSIGNED (v16uqi, unsigned char, 16, f4, >=);
  TEST_SIGNED (v8hi, short int, 8, f5, <=);
  TEST_UNSIGNED (v8uhi, unsigned short int, 8, f6, <=);
  TEST_SIGNED (v8hi, short int, 8, f7, >=);
  TEST_UNSIGNED (v8uhi, unsigned short int, 8, f8, >=);
  TEST_SIGNED (v4si, int, 4, f9, <=);
  TEST_UNSIGNED (v4usi, unsigned int, 4, f10, <=);
  TEST_SIGNED (v4si, int, 4, f11, >=);
  TEST_UNSIGNED (v4usi, unsigned int, 4, f12, >=);
  TEST_SIGNED (v2di, long long int, 2, f13, <=);
  TEST_UNSIGNED (v2udi, unsigned long long int, 2, f14, <=);
  TEST_SIGNED (v2di, long long int, 2, f15, >=);
  TEST_UNSIGNED (v2udi, unsigned long long int, 2, f16, >=);
}
