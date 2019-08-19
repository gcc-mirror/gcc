/* { dg-do run { target { aarch64_sve_hw } } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "cond_shift_1.c"

#define N 99

#define TEST_LOOP(TYPE, NAME, OP)				\
  {								\
    TYPE r[N], a[N], b[N];					\
    for (int i = 0; i < N; ++i)					\
      {								\
	a[i] = (i & 1 ? i : 3 * i);				\
	b[i] = (i >> 4) << (i & 15);				\
	asm volatile ("" ::: "memory");				\
      }								\
    test_##TYPE##_##NAME (r, a, b, N);				\
    for (int i = 0; i < N; ++i)					\
      if (r[i] != (TYPE) (a[i] > 20 ? b[i] OP 3 : b[i]))	\
	__builtin_abort ();					\
  }

int main ()
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
