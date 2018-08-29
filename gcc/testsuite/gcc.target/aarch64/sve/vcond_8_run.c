/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize -fno-trapping-math -ffinite-math-only" } */

#include "vcond_8.c"

#define N 187

#define TEST_LOOP(TYPE, CMPTYPE, OP)				\
  {								\
    TYPE dest[N], src[N];					\
    CMPTYPE cond[N];						\
    for (unsigned int i = 0; i < N; ++i)			\
      {								\
        src[i] = i * 3;						\
	cond[i] = i % 5;					\
      }								\
    f_##OP##_##TYPE (dest, cond, 3, src, 77, N);		\
    for (unsigned int i = 0; i < N; ++i)			\
      {								\
        TYPE if_false = i * 3;					\
	TYPE if_true = OP (if_false, (TYPE) 77);		\
	if (dest[i] != (i % 5 < 3 ? if_true : if_false))	\
	  __builtin_abort ();					\
      }								\
  }

int __attribute__ ((optimize (1)))
main (void)
{
  FOR_EACH_LOOP (TEST_LOOP);
  return 0;
}
