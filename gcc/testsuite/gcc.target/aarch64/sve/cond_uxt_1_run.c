/* { dg-do run { target { aarch64_sve_hw } } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "cond_uxt_1.c"

#define TEST_LOOP(TYPE, CONST)				\
  {							\
    TYPE r[NUM_ELEMS (TYPE)];				\
    TYPE a[NUM_ELEMS (TYPE)];				\
    TYPE b[NUM_ELEMS (TYPE)];				\
    for (int i = 0; i < NUM_ELEMS (TYPE); ++i)		\
      {							\
	a[i] = (i & 1 ? i : 3 * i);			\
	b[i] = (i >> 4) << (i & 15);			\
	asm volatile ("" ::: "memory");			\
      }							\
    test_##CONST##_##TYPE (r, a, b);			\
    for (int i = 0; i < NUM_ELEMS (TYPE); ++i)		\
      if (r[i] != (a[i] > 20 ? b[i] & CONST : b[i]))	\
	__builtin_abort ();				\
  }

int main ()
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
