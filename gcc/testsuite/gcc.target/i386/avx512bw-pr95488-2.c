/* { dg-do run } */
/* { dg-require-effective-target avx512bw } */
/* { dg-options "-O2 -mavx512bw -mavx512vl" } */

#ifndef CHECK
#define CHECK "avx512f-helper.h"
#endif

#include CHECK

#ifndef TEST
#define TEST avx512bw_test
#endif

#include "avx512bw-pr95488-1.c"

#define TEST_MULB(typeV, typeS, N, fn)		\
do						\
  {						\
    typeV v1, v2, res;				\
    typeS s1[N], s2[N], exp[N];		\
    int i,j;					\
						\
    for (i = 0; i < N; i++)			\
      {					\
	s1[i] = i * i;				\
	s2[i] = i + 20;			\
      }					\
    for (i = 0; i < N; i++)			\
      exp[i] = s1[i] * s2[i];			\
    v1 = *(typeV *)&s1[0];			\
    v2 = *(typeV *)&s2[0];			\
    res = fn (v1, v2);				\
    for (j = 0; j < N; j++)			\
      {					\
	if (res[j] != exp[j])			\
	  abort();				\
      }					\
}						\
while (0)

static void
TEST (void)
{
  TEST_MULB (v32qi, char, 32, mul_512);
  TEST_MULB (v32uqi, unsigned char, 32, umul_512);
}
