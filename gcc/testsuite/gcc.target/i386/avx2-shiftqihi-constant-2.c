/* PR target/95524 */
/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw -Wno-shift-count-overflow" } */

#ifndef CHECK
#define CHECK "avx512bw-check.h"
#endif

#include CHECK

#ifndef TEST
#define TEST avx512bw_test
#endif

typedef char v64qi  __attribute__ ((vector_size (64)));
typedef unsigned char v64uqi  __attribute__ ((vector_size (64)));

#define TEST_SHIFT(N)					\
  do							\
    {							\
      int i;						\
      for (i = 0; i < 64; i++)				\
	exp1.a[i] = op1.a[i] << N;			\
      res1.x = (__m512i) (((v64qi) op1.x) << N);	\
      if (check_union512i_b (res1, exp1.a))		\
	abort ();					\
							\
      for (i = 0; i < 64; i++)				\
	exp1.a[i] = op1.a[i] >> N;			\
      res1.x = (__m512i) (((v64qi) op1.x) >> N);	\
      if (check_union512i_b (res1, exp1.a))		\
	abort ();					\
							\
      for (i = 0; i < 64; i++)				\
	exp2.a[i] = op2.a[i] >> N;			\
      res2.x = (__m512i) (((v64uqi) op2.x >> N));	\
      if (check_union512i_ub (res2, exp2.a))		\
	abort ();					\
    }							\
  while (0)

static void
TEST (void)
{
  union512i_b op1, exp1, res1;
  union512i_ub op2, exp2, res2;
  for (int i = 0; i != 64; i++)
    {
      op2.a[i] = i * i;
      op1.a[i] = i * i + 200 * i;
    }
  TEST_SHIFT (0);
  TEST_SHIFT (1);
  TEST_SHIFT (2);
  TEST_SHIFT (3);
  TEST_SHIFT (4);
  TEST_SHIFT (5);
  TEST_SHIFT (6);
  TEST_SHIFT (7);
  TEST_SHIFT (8);
}

