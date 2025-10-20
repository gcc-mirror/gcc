#include "tree-vect.h"

int q[32];

int __attribute__((noipa))
foo ()
{
  int res = 0;
  for (int i = 0; i < 8; ++i)
    res += q[4*i] + q[4*i+1] + q[4*i+2] + q[4*i+3];
  return res;
}

int main()
{
  check_vect ();

  int sum = 0;
#pragma GCC novector
  for (int i = 0; i < 32; ++i)
    {
      q[i] = i;
      sum += i;
    }

  if (foo () != sum)
    abort ();
}

/* { dg-final { scan-tree-dump "vectorizing a reduction chain" "vect" } } */
/* { dg-final { scan-tree-dump "optimized: loop vectorized" "vect" } } */
