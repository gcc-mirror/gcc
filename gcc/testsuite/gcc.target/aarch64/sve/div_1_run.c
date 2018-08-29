/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize -fno-vect-cost-model --save-temps" } */

#include "div_1.c"

#define N 79

#define TEST_LOOP(TYPE)				\
  {						\
    TYPE dst[N], src1[N], src2[N];		\
    for (int i = 0; i < N; ++i)			\
      {						\
	src1[i] = i * 7 + i % 3;		\
	if (i % 11 > 7)				\
	  src1[i] = -src1[i];			\
	src2[i] = 5 + (i % 5);			\
	asm volatile ("" ::: "memory");		\
      }						\
    mod_##TYPE (dst, src1, src2, N);		\
    for (int i = 0; i < N; ++i)			\
      if (dst[i] != src1[i] / src2[i])		\
	__builtin_abort ();			\
  }

int
main (void)
{
  TEST_ALL (TEST_LOOP);
  return 0;
}
