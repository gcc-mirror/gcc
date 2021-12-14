/* { dg-do run } */
/* { dg-options "-O2 -mavx2 -mno-avx512f" } */
/* { dg-require-effective-target avx2 } */

#ifndef CHECK_H
#define CHECK_H "avx2-check.h"
#endif

#ifndef TEST
#define TEST avx2_test
#endif

#include CHECK_H

typedef long long V __attribute__((vector_size (32)));

#define TESTN(N) \
static V			\
__attribute__((noipa))		\
test##N (V x)			\
{				\
  return x >> N;		\
}

#define TESTS TESTN (63) TESTN (49) TESTN (32) TESTN (31) TESTN (18)
TESTS

struct
{
  int n;
  V (*fn) (V);
} tests[] = {
#undef TESTN
#define TESTN(N) { N, test##N },
  TESTS
};

static void
TEST (void)
{
  V a = (V) { 0xdeadbeefcafebabeULL, 0x123456789abcdef0ULL,
	      0x173a74be8a95134cULL, 0x817bae35ac0ebf12ULL };
  int i;
  for (i = 0; i < ARRAY_SIZE (tests); i++)
    {
      V c = tests[i].fn (a);
      if (c[0] != a[0] >> tests[i].n || c[1] != a[1] >> tests[i].n
	  || c[2] != a[2] >> tests[i].n || c[3] != a[3] >> tests[i].n)
	abort ();
    }
}
