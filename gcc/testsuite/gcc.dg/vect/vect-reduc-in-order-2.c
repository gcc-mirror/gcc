/* { dg-xfail-run-if "" { { i?86-*-* x86_64-*-* } && ia32 } } */
/* { dg-require-effective-target vect_double } */
/* { dg-add-options ieee } */
/* { dg-additional-options "-fno-fast-math" } */

#include "tree-vect.h"

#define N (VECTOR_BITS * 17)

double __attribute__ ((noinline, noclone))
reduc_plus_double (double *restrict a, int n)
{
  double res = 0.0;
  for (int i = 0; i < n; i++)
    for (int j = 0; j < N; j++)
      res += a[i];
  return res;
}

int __attribute__ ((optimize (1)))
main ()
{
  int n = 19;
  double a[N];
  double r = 0;
  for (int i = 0; i < N; i++)
    {
      a[i] = (i * 0.1) * (i & 1 ? 1 : -1);
      asm volatile ("" ::: "memory");
    }
  for (int i = 0; i < n; i++)
    for (int j = 0; j < N; j++)
      {
	r += a[i];
	asm volatile ("" ::: "memory");
      }
  double res = reduc_plus_double (a, n);
  if (res != r)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump {in-order double reduction not supported} "vect" } } */
/* { dg-final { scan-tree-dump-times {using an in-order \(fold-left\) reduction} 1 "vect" } } */
