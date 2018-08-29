/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize -fno-trapping-math -ffinite-math-only" } */

#include "vcond_9.c"

#define N 187

#define TEST_LOOP(TYPE, CMPTYPE, OP)				\
  {								\
    TYPE dest[N], src1[N], src2[N];				\
    CMPTYPE cond[N];						\
    for (unsigned int i = 0; i < N; ++i)			\
      {								\
        src1[i] = i * 4 - i % 7;				\
        src2[i] = i * 3 + 1;					\
	cond[i] = i % 5;					\
      }								\
    f_##OP##_##TYPE (dest, cond, 3, src1, src2, N);		\
    for (unsigned int i = 0; i < N; ++i)			\
      {								\
	TYPE src1v = i * 4 - i % 7;				\
        TYPE src2v = i * 3 + 1;					\
	TYPE if_true = OP (src1v, src2v);			\
	if (dest[i] != (i % 5 < 3 ? if_true : src2v))		\
	  __builtin_abort ();					\
      }								\
  }

int __attribute__ ((optimize (1)))
main (void)
{
  FOR_EACH_LOOP (TEST_LOOP);
  return 0;
}
