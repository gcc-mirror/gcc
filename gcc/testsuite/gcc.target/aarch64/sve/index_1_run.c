/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=256" } */

#include "index_1.c"

#define TEST_LOOP(TYPE, BASE, STEP, SUFFIX)	\
  {						\
    TYPE array[NUM_ELEMS (TYPE)] = {};		\
    loop_##TYPE##_##SUFFIX (array);		\
    for (int i = 0; i < NUM_ELEMS (TYPE); i++)	\
      if (array[i] != (TYPE) (BASE + i * STEP))	\
	__builtin_abort ();			\
  }

int __attribute__ ((optimize (1)))
main ()
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
