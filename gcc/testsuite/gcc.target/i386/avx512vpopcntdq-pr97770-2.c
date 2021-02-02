/* { dg-do run } */
/* { dg-options "-O2 -mavx512vpopcntdq" } */
/* { dg-require-effective-target avx512vpopcntdq } */

#define AVX512VPOPCNTDQ

#include "avx512f-helper.h"
#include "avx512vpopcntdq-pr97770-1.c"

#define SIZE_D AVX512F_LEN / 32
#define SIZE_Q AVX512F_LEN / 64


#define RTEST(TYPE, LEN, SIZE, MODE)			\
  do							\
    {							\
      TYPE res[SIZE], src[SIZE], res_ref[SIZE], v;	\
      int i, j, ret;					\
      for (i = 0; i < SIZE; i++)			\
	{						\
	  v = src[i] = i * 2 + 3;			\
	  ret = 0;					\
	  for (j = 0; j < sizeof(v) * 8; j++)		\
	    if ((v & ((TYPE)1 << (TYPE) j)))		\
	      ret++;					\
	  res_ref[i] = ret;				\
	}						\
      EVAL(popcount, MODE, LEN) (res, src);		\
	for (i = 0; i < SIZE; i++)			\
	  if (res[i] != res_ref[i])			\
	    abort ();					\
    }							\
  while (0)

void
TEST (void)
{
  RTEST (long long, AVX512F_LEN, SIZE_Q, q_);
  RTEST (int, AVX512F_LEN, SIZE_D, d_);
}
