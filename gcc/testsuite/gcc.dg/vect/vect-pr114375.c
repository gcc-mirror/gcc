/* { dg-additional-options "-mavx2" { target avx2_runtime } } */

#include "tree-vect.h"

int a[512];
int b[512];
int c[512];

void __attribute__((noipa))
foo(int * __restrict p)
{
  for (int i = 0; i < 64; ++i)
    {
      int tem = 2, tem2 = 2;
      if (a[4*i + 1])
        tem = p[4*i];
      if (a[4*i])
        tem2 = p[4*i + 2];
      b[2*i] = tem2;
      b[2*i+1] = tem;
      if (a[4*i + 2])
        tem = p[4*i + 1];
      if (a[4*i + 3])
        tem2 = p[4*i + 3];
      c[2*i] = tem2;
      c[2*i+1] = tem;
    }
}
int main()
{
  check_vect ();

#pragma GCC novector
  for (int i = 0; i < 512; ++i)
    a[i] = (i >> 1) & 1;

  foo (a);

  if (c[0] != 1 || c[1] != 0 || c[2] != 1 || c[3] != 0
      || b[0] != 2 || b[1] != 2 || b[2] != 2 || b[3] != 2)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target avx2 } } } */
