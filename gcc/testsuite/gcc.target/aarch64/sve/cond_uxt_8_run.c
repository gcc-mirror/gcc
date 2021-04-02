/* { dg-do run { target { aarch64_sve_hw } } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "cond_uxt_8.c"

#define TEST_LOOP(TYPE1, TYPE2, CONST, N)			\
  {								\
    TYPE1 a[N];							\
    TYPE2 r[N], b[N];						\
    for (int i = 0; i < N; ++i)					\
      {								\
	a[i] = (i & 1 ? -20 - i : 20 + i);			\
	b[i] = -5 - i;						\
	asm volatile ("" ::: "memory");				\
      }								\
    test_##CONST##_##TYPE1##_##TYPE2 (r, a, b);			\
    for (int i = 0; i < N; ++i)					\
      if (r[i] != (TYPE2) (a[i] > 20 ? b[i] & CONST : 0))	\
	__builtin_abort ();					\
  }

int main ()
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
