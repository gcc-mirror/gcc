/* PR target/96246 */
/* { dg-do run } */
/* { dg-require-effective-target avx512bw } */
/* { dg-options "-O2 -std=c++14 -mavx512bw" } */
/* { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } } */

#define AVX512BW

#include "avx512f-helper.h"

#include "avx512bw-pr96246-1.C"

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

void
test_512 (void)
{
  RUNTIME_TEST (v64qi, 64);
  RUNTIME_TEST (v32hi, 32);
  RUNTIME_TEST (v16si, 16);
  RUNTIME_TEST (v8di, 8);
  RUNTIME_TEST (v16sf, 16);
  RUNTIME_TEST (v8df, 8);
}
