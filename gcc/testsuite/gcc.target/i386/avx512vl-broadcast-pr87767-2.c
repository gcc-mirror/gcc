/* PR target/87767 */
/* { dg-do run } */
/* { dg-options "-O2 -mavx512f -mavx512dq -mavx512vl" } */
/* { dg-require-effective-target avx512dq } */
/* { dg-require-effective-target avx512vl } */

#define AVX512DQ
#define AVX512VL
#include "avx512f-helper.h"

#include "avx512vl-broadcast-pr87767-1.c"

#define RTEST(VTYPE, TYPE, N, OP_NAME, OP)		\
  do							\
    {							\
      TYPE exp[N], src[N];				\
      VTYPE res;					\
      for (int i = 0; i < N; i++)			\
	src[i] = i * i * 107;				\
      res = foo_##OP_NAME##_##VTYPE (*(VTYPE*)&src[0]);	\
      for (int i = 0; i < N; i ++)			\
	exp[i] = src[i] OP CONSTANT;			\
      for (int j = 0; j < N; j++)			\
	{						\
	  if (res[j] != exp[j])				\
	    abort();					\
	}						\
    }							\
  while (0)

void
test_256 (void)
{
  RTEST (v8si, int, 8, add, +);
  RTEST (v4di, long long, 4, add, +);
  RTEST (v8sf, float, 8, add, +);
  RTEST (v4df, double, 4, add, +);
  RTEST (v8si, int, 8, sub, -);
  RTEST (v4di, long long, 4, sub, -);
  RTEST (v8si, int, 8, mul, *);
  RTEST (v4di, long long, 4, mul, *);
  RTEST (v8sf, float, 8, mul, *);
  RTEST (v4df, double, 4, mul, *);
}

void
test_128 (void)
{
  RTEST (v4si, int, 4, add, +);
  RTEST (v2di, long long, 2, add, +);
  RTEST (v4sf, float, 4, add, +);
  RTEST (v2df, double, 2, add, +);
  RTEST (v4si, int, 4, sub, -);
  RTEST (v2di, long long, 2, sub, -);
  RTEST (v4si, int, 4, mul, *);
  RTEST (v2di, long long, 2, mul, *);
  RTEST (v4sf, float, 4, mul, *);
  RTEST (v2df, double, 2, mul, *);
}
