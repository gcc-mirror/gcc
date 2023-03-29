/* { dg-do run } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_perm } */

#include "tree-vect.h"

void __attribute__((noipa))
foo (int * __restrict__ a, int * __restrict__ b, int * __restrict__ c)
{
  int t = *c;
  for (int i = 0; i < 64; ++i)
    {
      b[i] = a[i] - t;
      t = a[i];
    }
}

int a[64], b[64];

int
main ()
{
  check_vect ();
  for (int i = 0; i < 64; ++i)
    {
      a[i] = i;
      __asm__ volatile ("" ::: "memory");
    }
  int c = 7;
  foo (a, b, &c);
  for (int i = 1; i < 64; ++i)
    if (b[i] != a[i] - a[i-1])
      abort ();
  if (b[0] != -7)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
