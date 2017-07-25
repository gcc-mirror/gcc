/* { dg-do run } */
/* { dg-require-effective-target vect_long_long } */

#include "tree-vect.h"

long long x[24];
long long y[16];
long long z[8];

void __attribute__((noinline)) foo()
{
  for (int i = 0; i < 8; ++i)
    {
      y[2*i] = x[3*i];
      y[2*i + 1] = x[3*i + 1];
      z[i] = 1;
    }
}

int main()
{
  check_vect ();

  for (int i = 0; i < 24; ++i)
    {
      x[i] = i;
      __asm__ volatile ("" : : : "memory");
    }
  foo ();
  for (int i = 0; i < 8; ++i)
    if (y[2*i] != 3*i || y[2*i+1] != 3*i + 1)
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
