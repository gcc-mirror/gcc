/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized" } */

#include "tree-vect.h"

#define N 50

#ifndef SIGNEDNESS
#define SIGNEDNESS unsigned
#endif
#ifndef BIAS
#define BIAS 0
#endif

void __attribute__ ((noipa))
f (SIGNEDNESS char *restrict a, SIGNEDNESS char *restrict b,
   SIGNEDNESS char *restrict c)
{
  for (__INTPTR_TYPE__ i = 0; i < N; ++i)
    a[i] = (b[i] + c[i] + BIAS) >> 1;
}

#define BASE1 ((SIGNEDNESS int) -1 < 0 ? -126 : 4)
#define BASE2 ((SIGNEDNESS int) -1 < 0 ? -101 : 26)

int
main (void)
{
  check_vect ();

  SIGNEDNESS char a[N], b[N], c[N];
  for (int i = 0; i < N; ++i)
    {
      b[i] = BASE1 + i * 5;
      c[i] = BASE2 + i * 4;
      asm volatile ("" ::: "memory");
    }
  f (a, b, c);
#pragma GCC novector
  for (int i = 0; i < N; ++i)
    if (a[i] != ((BASE1 + BASE2 + i * 9 + BIAS) >> 1))
      __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "vect_recog_average_pattern: detected" "vect" } } */
/* { dg-final { scan-tree-dump {\.AVG_FLOOR} "vect" { target vect_avg_qi } } } */
/* { dg-final { scan-tree-dump-not {vector\([^\n]*short} "optimized" { target vect_avg_qi } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" { target vect_avg_qi } } } */
