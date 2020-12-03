/* { dg-do run } */
/* { dg-require-effective-target avx512bw } */
/* { dg-require-effective-target avx512vl } */
/* { dg-options "-O2 -mavx512bw -mavx512vl" } */


#ifndef CHECK
#define CHECK "avx512f-check.h"
#endif

#define AVX512VL
#define AVX512BW

#include CHECK

#include "avx512bw-vec-set-1.c"

#define CALC_TEST(vtype, type, N, idx)				\
do								\
  {								\
    int i,val = idx * idx - idx * 3 + 16;			\
    type res[N],exp[N];						\
    vtype resv;							\
    for (i = 0; i < N; i++)					\
      {								\
	res[i] = i * i - i * 3 + 15;				\
	exp[i] = res[i];					\
      }								\
    exp[idx] = val;						\
    resv = foo_##vtype (*(vtype *)&res[0], val, idx);		\
    for (i = 0; i < N; i++)					\
      {								\
	if (resv[i] != exp[i])					\
	  abort ();						\
      }								\
  }								\
while (0)

static void
test_256 (void)
{
  CALC_TEST (v32qi, char, 32, 17);
  CALC_TEST (v16hi, short, 16, 9);
  CALC_TEST (v8si, int, 8, 3);
  CALC_TEST (v4di, long long, 4, 1);
}

static void
test_128 (void)
{
  CALC_TEST (v16qi, char, 16, 5);
  CALC_TEST (v8hi, short, 8, 6);
  CALC_TEST (v4si, int, 4, 2);
  CALC_TEST (v2di, long long, 2, 0);
}
