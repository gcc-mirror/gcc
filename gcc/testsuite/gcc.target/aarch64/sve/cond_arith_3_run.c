/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "cond_arith_3.c"

#define N 99

#undef TEST
#define TEST(TYPE, NAME, OP)					\
  {								\
    TYPE x[N], y[N], z[N], pred[N];				\
    for (int i = 0; i < N; ++i)					\
      {								\
	x[i] = -1;						\
	y[i] = i * i;						\
	z[i] = ((i + 2) % 3) * (i + 1);				\
	pred[i] = i % 3;					\
      }								\
    test_##TYPE##_##NAME (x, y, z, pred, N);			\
    for (int i = 0; i < N; ++i)					\
      {								\
	TYPE expected = i % 3 != 1 ? y[i] OP z[i] : 1;		\
	if (x[i] != expected)					\
	  __builtin_abort ();					\
	asm volatile ("" ::: "memory");				\
      }								\
  }

int
main (void)
{
  TEST_ALL
  return 0;
}
