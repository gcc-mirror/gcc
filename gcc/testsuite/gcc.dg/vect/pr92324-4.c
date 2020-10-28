#include "tree-vect.h"

unsigned a[1024];
int gres1, gres2;

int __attribute__((noipa))
foo (int n)
{
  int res1 = 0;
  int res2 = 0;
  for (int i = 0; i < n; ++i)
    {
      res1 = res1 > a[2*i] ? res1 : a[2*i];
      res2 = res2 > a[2*i+1] ? res2 : a[2*i+1];
    }
  gres1 = res1;
  gres2 = res2;
}

int main ()
{
  check_vect ();
  a[30] = (unsigned)__INT_MAX__ + 1;
  a[31] = (unsigned)__INT_MAX__ + 1;
  foo (16);
  if (gres1 != -__INT_MAX__ - 1
      || gres2 != -__INT_MAX__ - 1)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 0 "vect" } } */
