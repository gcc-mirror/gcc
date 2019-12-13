#include "tree-vect.h"

int a[1024];
unsigned b[1024];

int __attribute__((noipa))
foo (int n)
{
  int res = 0;
  for (int i = 0; i < n; ++i)
    {
      res = res > a[i] ? res : a[i];
      res = res > b[i] ? res : b[i];
    }
  return res;
}

int main ()
{
  check_vect ();
  b[3] = (unsigned)__INT_MAX__ + 1;
  if (foo (4) != -__INT_MAX__ - 1)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-not "vectorized \[1-9\] loops" "vect" } } */
