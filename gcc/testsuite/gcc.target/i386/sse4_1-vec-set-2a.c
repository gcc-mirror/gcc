/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */


#ifndef CHECK
#define CHECK "sse4_1-check.h"
#endif

#ifndef TEST
#define TEST sse4_1_test
#endif

#include CHECK

#include "sse4_1-vec-set-1a.c"

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
TEST (void)
{
  CALC_TEST (v4qi, char, 4, 2);
  CALC_TEST (v2hi, short, 2, 1);
}
