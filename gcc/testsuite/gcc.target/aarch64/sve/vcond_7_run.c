/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "vcond_7.c"

#define TEST_CONST_LOOP(NAME, SUFFIX, TYPE, CONST)			\
  {									\
    for (int i = 0; i < N; ++i)						\
      {									\
	dst[i] = i * 3;							\
	src[i] = i % (CONST + 3);					\
      }									\
    NAME##_##SUFFIX##_##TYPE (dst, src);				\
    for (int i = 0; i < N; ++i)						\
      if (dst[i] != (NAME (src[i], CONST) ? (TYPE) 1 : (TYPE) (i * 3)))	\
	__builtin_abort ();						\
  }

#define TEST_LOOPS(NAME, TYPE, CONST1, CONST2)				\
  {									\
    TYPE dst[N], src[N];						\
    for (int i = 0; i < N; ++i)						\
      {									\
	dst[i] = i * 2;							\
	src[i] = i % 5;							\
      }									\
    NAME##_var_##TYPE (dst, src, 3);					\
    for (int i = 0; i < N; ++i)						\
      if (dst[i] != (NAME (src[i], 3) ? (TYPE) 3 : (TYPE) (i * 2)))	\
	__builtin_abort ();						\
    TEST_CONST_LOOP (NAME, const1, TYPE, CONST1)			\
    TEST_CONST_LOOP (NAME, const2, TYPE, CONST2)			\
  }

int __attribute__ ((noipa))
main (void)
{
  FOR_EACH_TYPE (TEST_LOOPS);
  return 0;
}
