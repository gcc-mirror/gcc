/* PR target/96246 */
/* { dg-do run } */
/* { dg-require-effective-target avx512bw } */
/* { dg-require-effective-target avx512vl } */
/* { dg-options "-Ofast -mavx512bw -mavx512vl" } */

#ifndef CHECK
#define CHECK "avx512f-helper.h"
#endif

#include CHECK

#ifndef TEST
#define TEST avx512bw_test
#endif

#include "avx512vl-pr96246-1.c"

#define RUNTIME_TEST(vtype, num)			\
  do							\
    {							\
      vtype a, b, c, d;					\
      vtype res;					\
      for (int i = 0; i != num; i++)			\
	{						\
	  a[i] = i * 2;					\
	  b[i] = i * i - 5;				\
	  c[i] = 1;					\
	  d[i] = 0;					\
	}						\
      res = foo_##vtype (a, b, c, d);			\
      for (int i = 0; i != num; i++)			\
	if (res [i] != (a[i] > b[i] ? c[i] : d[i]))	\
	  __builtin_abort ();				\
    }							\
  while (0)

static void
__attribute__ ((optimize (0)))
TEST (void)
{
  RUNTIME_TEST (v16qi, 16);
  RUNTIME_TEST (v32qi, 32);
  RUNTIME_TEST (v16hi, 16);
  RUNTIME_TEST (v4si, 4);
  RUNTIME_TEST (v8si, 8);
  RUNTIME_TEST (v4sf, 4);
  RUNTIME_TEST (v8sf, 8);
  RUNTIME_TEST (v4di, 4);
  RUNTIME_TEST (v4df, 4);
}
