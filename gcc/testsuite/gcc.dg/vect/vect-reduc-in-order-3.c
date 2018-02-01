/* { dg-xfail-run-if "" { { i?86-*-* x86_64-*-* } && ia32 } } */
/* { dg-require-effective-target vect_double } */
/* { dg-add-options ieee } */
/* { dg-additional-options "-fno-fast-math" } */

#include "tree-vect.h"

#define N (VECTOR_BITS * 17)

double __attribute__ ((noinline, noclone))
reduc_plus_double (double *a)
{
  double r = 0;
  for (int i = 0; i < N; i += 4)
    {
      r += a[i] * 2.0;
      r += a[i + 1] * 3.0;
      r += a[i + 2] * 4.0;
      r += a[i + 3] * 5.0;
    }
  return r;
}

int __attribute__ ((optimize (1)))
main ()
{
  double a[N];
  double r = 0;
  for (int i = 0; i < N; i++)
    {
      a[i] = (i * 0.1) * (i & 1 ? 1 : -1);
      r += a[i] * (i % 4 + 2);
      asm volatile ("" ::: "memory");
    }
  double res = reduc_plus_double (a);
  if (res != r)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times {using an in-order \(fold-left\) reduction} 1 "vect" } } */
/* { dg-final { scan-tree-dump-times {vectorizing stmts using SLP} 1 "vect" } } */
