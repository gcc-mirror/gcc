/* { dg-additional-options "-fopenmp-simd -fno-tree-vectorize" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { target i?86-*-* x86_64-*-* } } } */

#include "tree-vect.h"

__attribute__((noipa)) int
foo (int s, int *p)
{
  int r = 0, l = 0, i;
  #pragma omp simd reduction (+:r) linear(l)
  for (i = 0; i < 10000; i += s)
    {
      p[l++] = i;
      r += i * 3;
    }
  return r;
}

int p[10000 / 78];

int
main ()
{
  int i, r;
  check_vect ();
  r = foo (78, p);
  for (i = 0; i < 10000 / 78; i++)
    if (p[i] != 78 * i)
      abort ();
  if (r != (10000 / 78) * (10000 / 78 + 1) / 2 * 78 * 3)
    abort ();
  r = foo (87, p);
  for (i = 0; i < 10000 / 87; i++)
    if (p[i] != 87 * i)
      abort ();
  if (r != (10000 / 87) * (10000 / 87 + 1) / 2 * 87 * 3)
    abort ();
  return 0;
}
