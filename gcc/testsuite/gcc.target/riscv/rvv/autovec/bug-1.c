/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64d -mrvv-max-lmul=m8 -mrvv-vector-bits=zvl  -fno-vect-cost-model -O3 -fdump-tree-optimized" } */

#include <assert.h>

#define N 64

typedef struct
{
  unsigned char a;
  unsigned char b;
} s;

int
main1 (s *arr)
{
  s *ptr = arr;
  s res[N];
  int i;

  for (i = 0; i < N; i++)
    {
      res[i].a = ptr->b - ptr->a;
      res[i].b = ptr->b + ptr->a;
      ptr++;
    }
    /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (res[i].a != arr[i].b - arr[i].a || res[i].b != arr[i].a + arr[i].b)
	assert (0);
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "\.VEC_PERM" 3 "optimized" } } */
/* { dg-final { scan-assembler-not {vmsltu\.vi} } } */
