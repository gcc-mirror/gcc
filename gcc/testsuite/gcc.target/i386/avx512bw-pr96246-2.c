/* PR target/96246 */
/* { dg-do run } */
/* { dg-require-effective-target avx512bw } */
/* { dg-options "-Ofast -mavx512bw" } */

#ifndef CHECK
#define CHECK "avx512f-helper.h"
#endif

#include CHECK

#ifndef TEST
#define TEST avx512bw_test
#endif

#include "avx512bw-pr96246-1.c"

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
  RUNTIME_TEST (v64qi, 64);
  RUNTIME_TEST (v32hi, 32);
  RUNTIME_TEST (v16si, 16);
  RUNTIME_TEST (v8di, 8);
  RUNTIME_TEST (v16sf, 16);
  RUNTIME_TEST (v8df, 8);
}
