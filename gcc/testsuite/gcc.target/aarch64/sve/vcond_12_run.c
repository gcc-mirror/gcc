/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math" } */

#include "vcond_12.c"

#define TEST_LOOP(TYPE, CMPTYPE, OP)				\
  {								\
    TYPE dest[N];						\
    CMPTYPE cond[N];						\
    for (unsigned int i = 0; i < N; ++i)			\
      cond[i] = i % 5;						\
    TYPE src2v = 14;						\
    TYPE elsev = 17;						\
    f_##OP##_##TYPE (dest, cond, 3, src2v, elsev);		\
    TYPE induc = 0;						\
    for (unsigned int i = 0; i < N; ++i)			\
      {								\
	TYPE if_true = OP (induc, src2v);			\
	if (dest[i] != (i % 5 < 3 ? if_true : elsev))		\
	  __builtin_abort ();					\
	induc += 1;						\
      }								\
  }

int __attribute__ ((optimize (1)))
main (void)
{
  FOR_EACH_LOOP (TEST_LOOP);
  return 0;
}
