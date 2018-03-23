/* { dg-do run { target { aarch64_sve_hw } } } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math" } */

#include "maxmin_1.c"

#define TEST_LOOP(TYPE, NAME, CMP_OP)			\
  {							\
    TYPE a[NUM_ELEMS (TYPE)];				\
    TYPE b[NUM_ELEMS (TYPE)];				\
    TYPE r[NUM_ELEMS (TYPE)];				\
    for (int i = 0; i < NUM_ELEMS (TYPE); i++)		\
      {							\
	a[i] = ((i * 2) % 3) * (i & 1 ? 1 : -1);	\
	b[i] = (1 + (i % 4)) * (i & 1 ? -1 : 1);	\
	asm volatile ("" ::: "memory");			\
      }							\
    fun_##NAME##_##TYPE (r, a, b);			\
    for (int i = 0; i < NUM_ELEMS (TYPE); i++)		\
      if (r[i] != (a[i] CMP_OP b[i] ? a[i] : b[i]))	\
	__builtin_abort ();				\
  }

int main ()
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
