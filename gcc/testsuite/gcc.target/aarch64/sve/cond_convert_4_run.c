/* { dg-do run { target { aarch64_sve_hw } } } */
/* { dg-options "-O2 -ftree-vectorize -ftrapping-math" } */

#include "cond_convert_4.c"

#define N 99

#define TEST_LOOP(FLOAT_TYPE, INT_TYPE)				\
  {								\
    INT_TYPE r[N], b[N], pred[N];				\
    FLOAT_TYPE a[N];						\
    for (int i = 0; i < N; ++i)					\
      {								\
	a[i] = (i & 1 ? i : 3 * i) * (i % 3 == 0 ? 1 : -1);	\
	b[i] = (i % 9) * (i % 7 + 1);				\
	pred[i] = (i % 7 < 4);					\
	asm volatile ("" ::: "memory");				\
      }								\
    test_##INT_TYPE (r, a, b, pred, N);				\
    for (int i = 0; i < N; ++i)					\
      if (r[i] != (pred[i] ? (INT_TYPE) a[i] : b[i]))		\
	__builtin_abort ();					\
  }

int main ()
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
