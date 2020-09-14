/* PR target/87767 */
/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-helper.h"

#include "avx512f-broadcast-pr87767-5.c"

#define RTEST(VTYPE, TYPE, N, OP_NAME, OP1, OP2)				\
  do									\
    {									\
      TYPE exp[N], src[N];						\
      VTYPE res;							\
      for (int i = 0; i < N; i++)					\
	{								\
	  src[i] = i * i * 107;						\
	}								\
      res = foo_##OP_NAME##_##VTYPE (*(VTYPE*)&src[0]);			\
      for (int i = 0; i < N; i ++)					\
	exp[i] = (OP1 src[i]) OP2 CONSTANT;				\
      for (int j = 0; j < N; j++)					\
	{								\
	  if (res[j] != exp[j])						\
	    abort();							\
	}								\
    }									\
  while (0)

void
test_512 (void)
{
  RTEST (v16si, int, 16, andnot, ~, &);
  RTEST (v8di, long long, 8, andnot, ~, &);
  RTEST (v16si, int, 16, and,, &);
  RTEST (v8di, long long, 8, and,, &);
  RTEST (v16si, int, 16, or,, |);
  RTEST (v8di, long long, 8, or,, |);
  RTEST (v16si, int, 16, xor,, ^);
  RTEST (v8di, long long, 8, xor,, ^);
}
