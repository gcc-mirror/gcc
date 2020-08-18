/* PR target/96246 */
/* { dg-do run } */
/* { dg-require-effective-target avx512bw } */
/* { dg-require-effective-target avx512vl } */
/* { dg-options "-O2 -std=c++14 -mavx512bw -mavx512vl" } */

#include "avx512vl-pr96246-1.C"

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

int
main (void)
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
  return 0;
}
