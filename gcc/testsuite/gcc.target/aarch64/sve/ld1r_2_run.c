/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O3 -fno-tree-loop-distribute-patterns" } */

#include "ld1r_2.c"

#define TEST_LOAD_BROADCAST(TYPE)		\
  {						\
    TYPE v[NUM_ELEMS (TYPE)];			\
    TYPE val = 99;				\
    set_##TYPE (v, &val);			\
    for (int i = 0; i < NUM_ELEMS (TYPE); i++)	\
      {						\
	if (v[i] != (TYPE) 99)			\
	  __builtin_abort ();			\
	asm volatile ("" ::: "memory");		\
      }						\
  }

#define TEST_LOAD_BROADCAST_IMM(TYPE, IMM, SUFFIX)	\
  {							\
    TYPE v[NUM_ELEMS (TYPE)];				\
    set_##TYPE##_##SUFFIX (v);				\
    for (int i = 0; i < NUM_ELEMS (TYPE); i++ )		\
      {							\
	if (v[i] != (TYPE) IMM)				\
	  __builtin_abort ();				\
	asm volatile ("" ::: "memory");			\
      }							\
  }

int __attribute__ ((optimize (1)))
main (int argc, char **argv)
{
  FOR_EACH_LOAD_BROADCAST (TEST_LOAD_BROADCAST)
  FOR_EACH_LOAD_BROADCAST_IMM (TEST_LOAD_BROADCAST_IMM)

  return 0;
}
