/* { dg-xfail-run-if "" { { i?86-*-* x86_64-*-* } && ia32 } } */
/* { dg-require-effective-target vect_double } */
/* { dg-add-options ieee } */
/* { dg-additional-options "-fno-fast-math" } */

#include "tree-vect.h"

#define N (VECTOR_BITS * 17)

double __attribute__ ((noinline, noclone))
reduc_plus_double (double *a)
{
  double r1 = 0;
  double r2 = 0;
  double r3 = 0;
  double r4 = 0;
  for (int i = 0; i < N; i += 4)
    {
      r1 += a[i];
      r2 += a[i + 1];
      r3 += a[i + 2];
      r4 += a[i + 3];
    }
  return r1 * r2 * r3 * r4;
}

int __attribute__ ((optimize (1)))
main ()
{
  double a[N];
  double r[4] = {};
  for (int i = 0; i < N; i++)
    {
      a[i] = (i * 0.1) * (i & 1 ? 1 : -1);
      r[i % 4] += a[i];
      asm volatile ("" ::: "memory");
    }
  double res = reduc_plus_double (a);
  if (res != r[0] * r[1] * r[2] * r[3])
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "VECT_PERM_EXPR" 0 "vect" } } */
