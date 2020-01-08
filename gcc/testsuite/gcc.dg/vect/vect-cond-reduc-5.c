#include "tree-vect.h"

#define N 512

int a[N], b[N];

int __attribute__((noipa))
foo (int aval, int bval)
{
  int i, res = 0;
  for (i=0; i<N; i++)
  {
    if (a[i] != 0)
      res = aval;
    if (b[i] != 0)
      res = bval;
  }
  return res;
}

int main()
{
  check_vect ();
  if (foo (1, 2) != 0)
    abort ();
  a[3] = 1;
  b[4] = 1;
  if (foo (1, 2) != 2)
    abort ();
  a[7] = 1;
  if (foo (1, 2) != 1)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_condition } } } */
