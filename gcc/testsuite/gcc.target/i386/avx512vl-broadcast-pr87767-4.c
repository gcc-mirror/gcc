/* PR target/87767 */
/* { dg-do run } */
/* { dg-options "-O2 -mavx512f -mavx512vl" } */
/* { dg-require-effective-target avx512vl } */

#define AVX512VL
#include "avx512f-helper.h"

#include "avx512vl-broadcast-pr87767-3.c"

#define RTEST(VTYPE, TYPE, N, OP_NAME, OP1, OP2)				\
  do									\
    {									\
      TYPE exp[N], src1[N], src2[N];					\
      VTYPE res;							\
      for (int i = 0; i < N; i++)					\
	{								\
	  src1[i] = i * i * 107.2f;					\
	  src2[i] = i * 2.f - 404.f;					\
	}								\
      res = foo_##OP_NAME##_##VTYPE (*(VTYPE*)&src1[0], *(VTYPE*)&src2[0]); \
      for (int i = 0; i < N; i ++)					\
	exp[i] = (OP1 src1[i] * src2[i]) OP2 CONSTANT;			\
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
  RTEST (v8sf, float, 8, fma,, +);
  RTEST (v4df, double, 4, fma,, +);
  RTEST (v8sf, float, 8, fms,, -);
  RTEST (v4df, double, 4, fms,, -);
  RTEST (v8sf, float, 8, fnma,-, +);
  RTEST (v4df, double, 4, fnma,-, +);
  RTEST (v8sf, float, 8, fnms,-, -);
  RTEST (v4df, double, 4, fnms,-, -);
}

void
test_128 (void)
{
  RTEST (v4sf, float, 4, fma,, +);
  RTEST (v2df, double, 2, fma,, +);
  RTEST (v4sf, float, 4, fms,, -);
  RTEST (v2df, double, 2, fms,, -);
  RTEST (v4sf, float, 4, fnma,-, +);
  RTEST (v2df, double, 2, fnma,-, +);
  RTEST (v4sf, float, 4, fnms,-, -);
  RTEST (v2df, double, 2, fnms,-, -);
}
