/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

int x[1024], y[1024], z[1024];

int __attribute__((noinline,noclone))
foo (int n)
{
  int sum = 0;
  /* Can vectorize this.  */
  for (int i = 0; i < n; ++i)
    sum = (y[i] - (x[i] - sum));
  return sum;
}

int __attribute__((noinline,noclone))
bar (int n)
{
  int sum = 0;
  /* Cannot vectorize this, sum is negated.  */
  for (int i = 0; i < n; ++i)
    sum = z[i] - (y[i] - (x[i] - sum));
  return sum;
}

int
main()
{
  check_vect ();
  for (int i = 0; i < 1024; ++i)
    {
      x[i] = i;
      y[i] = i + 1;
      z[i] = 0;
      __asm__ volatile ("" : : : "memory");
    }
  if (foo (1024) != 1024)
    __builtin_abort ();
  if (bar (1023) != -1 || bar (1024) != 0)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
