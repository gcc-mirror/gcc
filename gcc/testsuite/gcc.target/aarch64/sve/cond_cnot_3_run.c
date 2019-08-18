/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "cond_cnot_3.c"

#define N 99

#define TEST_LOOP(TYPE)						\
  {								\
    TYPE r[N], a[N], b[N];					\
    for (int i = 0; i < N; ++i)					\
      {								\
	a[i] = (i % 3) < (i % 5);				\
	b[i] = i % 7 < 3;					\
	asm volatile ("" ::: "memory");				\
      }								\
    test_##TYPE (r, a, b, N);					\
    for (int i = 0; i < N; ++i)					\
      {								\
	TYPE expected = a[i] == 0 ? !b[i] : 127;		\
	if (r[i] != expected)					\
	  __builtin_abort ();					\
	asm volatile ("" ::: "memory");				\
      }								\
  }

int
main (void)
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
