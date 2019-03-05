/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#ifndef CHECK
#define CHECK "avx2-check.h"
#endif

#ifndef TEST
#define TEST avx2_test
#endif

#include CHECK

#include "avx2-pr88547-1.c"

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
  TEST_SIGNED (v32qi, signed char, 32, f1, <=);
  TEST_UNSIGNED (v32uqi, unsigned char, 32, f2, <=);
  TEST_SIGNED (v32qi, signed char, 32, f3, >=);
  TEST_UNSIGNED (v32uqi, unsigned char, 32, f4, >=);
  TEST_SIGNED (v16hi, short int, 16, f5, <=);
  TEST_UNSIGNED (v16uhi, unsigned short int, 16, f6, <=);
  TEST_SIGNED (v16hi, short int, 16, f7, >=);
  TEST_UNSIGNED (v16uhi, unsigned short int, 16, f8, >=);
  TEST_SIGNED (v8si, int, 8, f9, <=);
  TEST_UNSIGNED (v8usi, unsigned int, 8, f10, <=);
  TEST_SIGNED (v8si, int, 8, f11, >=);
  TEST_UNSIGNED (v8usi, unsigned int, 8, f12, >=);
  TEST_SIGNED (v4di, long long int, 4, f13, <=);
  TEST_UNSIGNED (v4udi, unsigned long long int, 4, f14, <=);
  TEST_SIGNED (v4di, long long int, 4, f15, >=);
  TEST_UNSIGNED (v4udi, unsigned long long int, 4, f16, >=);
}
