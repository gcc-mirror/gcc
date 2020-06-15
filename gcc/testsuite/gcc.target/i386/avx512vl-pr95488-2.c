/* { dg-do run } */
/* { dg-require-effective-target avx512bw } */
/* { dg-require-effective-target avx512vl } */
/* { dg-options "-O2 -mavx512bw -mavx512vl" } */

#ifndef CHECK
#define CHECK "avx512f-helper.h"
#endif

#include CHECK

#ifndef TEST
#define TEST avx512bw_test
#endif

#include "avx512vl-pr95488-1.c"

#define TEST_MULB(typeV, typeS, N, fn)		\
do						\
  {						\
    typeV v1, v2, res;				\
    int i,j;					\
    typeS s1[N], s2[N], exp[N];		\
						\
    for (i = 0; i < N; i++)			\
      {					\
	s1[i] = i * i;				\
	s2[i] = i + 20;			\
      }					\
    for (i = 0; i < N; i++)			\
      exp[i] = s1[i] * s2[i];			\
    v1 = *(typeV *)s1;				\
    v2 = *(typeV *)s2;				\
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
  TEST_MULB(v8qi, char, 8, mul_128);
  TEST_MULB(v8uqi, unsigned char, 8, umul_128);
  TEST_MULB(v16qi, char, 16, mul_256);
  TEST_MULB(v16uqi, unsigned char, 16, umul_256);
}
