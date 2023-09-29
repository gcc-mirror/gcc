/* { dg-additional-options "-fdump-tree-optimized" } */

#include "tree-vect.h"

#define N 4

int a[N];

int __attribute__ ((noipa))
f1 (void)
{
  int b[N] = { 1, 0, 0, 0 }, res = 0;
  for (int i = 0; i < N; ++i)
    res += a[i] * b[i];
  return res;
}

int __attribute__ ((noipa))
f2 (void)
{
  int b[N] = { 0, 1, 0, 0 }, res = 0;
  for (int i = 0; i < N; ++i)
    res += a[i] * b[i];
  return res;
}

int __attribute__ ((noipa))
f3 (void)
{
  int b[N] = { 0, 0, 0, 1 }, res = 0;
  for (int i = 0; i < N; ++i)
    res += a[i] * b[i];
  return res;
}

int
main ()
{
  check_vect ();

  for (int i = 0; i < N; ++i)
    a[i] = 0xe0 + i;

  if (f1 () != a[0]
      || f2 () != a[1]
      || f3 () != a[N - 1])
    __builtin_abort ();

  return 0;
}

/* ??? We need more constant folding for this to work with fully-masked
   loops.  */
/* { dg-final { scan-tree-dump-not {REDUC_PLUS} "optimized" { xfail { aarch64_sve || riscv_v } } } } */
