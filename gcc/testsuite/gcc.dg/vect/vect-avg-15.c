/* { dg-additional-options "-O3" } */
/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

#define N 80

void __attribute__ ((noipa))
f (signed char *restrict a, signed char *restrict b,
   signed char *restrict c, int n, int step)
{
  for (int j = 0; j < n; ++j)
    {
      for (int i = 0; i < 16; ++i)
	a[i] = (b[i] + c[i]) >> 1;
      a += step;
      b += step;
      c += step;
    }
}

#define BASE1 -126
#define BASE2 -42

signed char a[N], b[N], c[N];

int
main (void)
{
  check_vect ();

  for (int i = 0; i < N; ++i)
    {
      a[i] = i;
      b[i] = BASE1 + i * 3;
      c[i] = BASE2 + i * 2;
      asm volatile ("" ::: "memory");
    }
  f (a, b, c, N / 20, 20);
#pragma GCC novector
  for (int i = 0; i < N; ++i)
    {
      int d = (BASE1 + BASE2 + i * 5) >> 1;
      if (a[i] != (i % 20 < 16 ? d : i))
	__builtin_abort ();
    }
  return 0;
}

/* { dg-final { scan-tree-dump "vect_recog_average_pattern: detected" "vect" } } */
/* { dg-final { scan-tree-dump {\.AVG_FLOOR} "vect" { target vect_avg_qi } } } */
/* { dg-final { scan-tree-dump "Loop contains only SLP stmts" "vect" { target vect_avg_qi } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" { target vect_avg_qi } } } */
