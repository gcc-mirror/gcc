/* { dg-do run } */
/* { dg-require-effective-target avx512bw } */
/* { dg-options "-O2 -mavx512bw" } */


#ifndef CHECK
#define CHECK "avx512f-check.h"
#endif

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
test_512 (void)
{
  CALC_TEST (v64qi, char, 64, 50);
  CALC_TEST (v32hi, short, 32, 30);
  CALC_TEST (v16si, int, 16, 15);
  CALC_TEST (v8di, long long, 8, 7);
}
