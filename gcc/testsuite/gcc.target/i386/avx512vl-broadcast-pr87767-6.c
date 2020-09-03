/* PR target/87767 */
/* { dg-do run } */
/* { dg-options "-O2 -mavx512f -mavx512vl" } */
/* { dg-require-effective-target avx512vl } */

#define AVX512VL
#include "avx512f-helper.h"

#include "avx512vl-broadcast-pr87767-5.c"

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
test_256 (void)
{
  RTEST (v8si, int, 8, andnot, ~, &);
  RTEST (v4di, long long, 4, andnot, ~, &);
  RTEST (v8si, int, 8, and,, &);
  RTEST (v4di, long long, 4, and,, &);
  RTEST (v8si, int, 8, or,, |);
  RTEST (v4di, long long, 4, or,, |);
  RTEST (v8si, int, 8, xor,, ^);
  RTEST (v4di, long long, 4, xor,, ^);
}

void
test_128 (void)
{
  RTEST (v4si, int, 4, andnot, ~, &);
  RTEST (v2di, long long, 2, andnot, ~, &);
  RTEST (v4si, int, 4, and,, &);
  RTEST (v2di, long long, 2, and,, &);
  RTEST (v4si, int, 4, or,, |);
  RTEST (v2di, long long, 2, or,, |);
  RTEST (v4si, int, 4, xor,, ^);
  RTEST (v2di, long long, 2, xor,, ^);
}
