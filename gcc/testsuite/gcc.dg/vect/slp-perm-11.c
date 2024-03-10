/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

int a[64];
int b[128];

void __attribute__((noinline, noclone))
foo (int s)
{
  int i;
  for (i = 0; i < 32; ++i)
    {
      a[2*i] = b[i*s+1];
      a[2*i+1] = b[i*s];
    }
}

int main ()
{
  int i;
  check_vect ();
  for (i = 0; i < 128; ++i)
    {
      b[i] = i;
      __asm__ volatile ("");
    }
  foo (4);
#pragma GCC novector
  for (i = 0; i < 64; ++i)
    if (a[i] != (4*(i/2) + (i & 1) ^ 1))
      abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target vect_perm } } } */
