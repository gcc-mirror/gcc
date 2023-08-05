/* { dg-do run } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_perm } */

#include "tree-vect.h"

void __attribute__((noipa))
foo (int * __restrict__ a, short * __restrict__ b, int * __restrict__ c)
{
  int t1 = *c;
  int t2 = *c;
  for (int i = 0; i < 64; i+=2)
    {
      b[i] = a[i] - t1;
      t1 = a[i];
      b[i+1] = a[i+1] - t2;
      t2 = a[i+1];
    }
}

int a[64];
short b[64];

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
#pragma GCC novector
  for (int i = 2; i < 64; i+=2)
    if (b[i] != a[i] - a[i-2]
	|| b[i+1] != a[i+1] - a[i-1])
      abort ();
  if (b[0] != -7 || b[1] != -6)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
