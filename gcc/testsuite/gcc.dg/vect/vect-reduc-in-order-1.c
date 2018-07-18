/* { dg-xfail-run-if "" { { i?86-*-* x86_64-*-* } && ia32 } } */
/* { dg-require-effective-target vect_double } */
/* { dg-add-options ieee } */
/* { dg-additional-options "-fno-fast-math" } */

#include "tree-vect.h"

#define N (VECTOR_BITS * 17)

double __attribute__ ((noinline, noclone))
reduc_plus_double (double *a, double *b)
{
  double r = 0, q = 3;
  for (int i = 0; i < N; i++)
    {
      r += a[i];
      q -= b[i];
    }
  return r * q;
}

int __attribute__ ((optimize (1)))
main ()
{
  double a[N];
  double b[N];
  double r = 0, q = 3;
  for (int i = 0; i < N; i++)
    {
      a[i] = (i * 0.1) * (i & 1 ? 1 : -1);
      b[i] = (i * 0.3) * (i & 1 ? 1 : -1);
      r += a[i];
      q -= b[i];
      asm volatile ("" ::: "memory");
    }
  double res = reduc_plus_double (a, b);
  if (res != r * q)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times {using an in-order \(fold-left\) reduction} 2 "vect" } } */
