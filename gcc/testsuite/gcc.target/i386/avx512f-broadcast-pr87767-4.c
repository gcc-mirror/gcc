/* PR target/87767 */
/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-helper.h"

#include "avx512f-broadcast-pr87767-3.c"

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
test_512 (void)
{
  RTEST (v16sf, float, 16, fma,, +);
  RTEST (v8df, double, 8, fma,, +);
  RTEST (v16sf, float, 16, fms,, -);
  RTEST (v8df, double, 8, fms,, -);
  RTEST (v16sf, float, 16, fnma,-, +);
  RTEST (v8df, double, 8, fnma,-, +);
  RTEST (v16sf, float, 16, fnms,-, -);
  RTEST (v8df, double, 8, fnms,-, -);
}
