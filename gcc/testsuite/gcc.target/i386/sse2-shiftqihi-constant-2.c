/* PR target/95524 */
/* { dg-do run } */
/* { dg-options "-O2 -msse2 -Wno-shift-count-overflow" } */

#ifndef CHECK
#define CHECK "sse2-check.h"
#endif

#include CHECK

#ifndef TEST
#define TEST sse2_test
#endif

typedef char v16qi  __attribute__ ((vector_size (16)));
typedef unsigned char v16uqi  __attribute__ ((vector_size (16)));

#define TEST_SHIFT(N)					\
  do							\
    {							\
      int i;						\
      for (i = 0; i < 16; i++)				\
	exp1.a[i] = op1.a[i] << N;			\
      res1.x = (__m128i) (((v16qi) op1.x) << N);	\
      if (check_union128i_b (res1, exp1.a))		\
	abort ();					\
							\
      for (i = 0; i < 16; i++)				\
	exp1.a[i] = op1.a[i] >> N;			\
      res1.x = (__m128i) (((v16qi) op1.x) >> N);	\
      if (check_union128i_b (res1, exp1.a))		\
	abort ();					\
							\
      for (i = 0; i < 16; i++)				\
	exp2.a[i] = op2.a[i] >> N;			\
      res2.x = (__m128i) (((v16uqi) op2.x >> N));	\
      if (check_union128i_ub (res2, exp2.a))		\
	abort ();					\
    }							\
  while (0)

static void
TEST (void)
{
  union128i_b op1, exp1, res1;
  union128i_ub op2, exp2, res2;
  for (int i = 0; i != 16; i++)
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

