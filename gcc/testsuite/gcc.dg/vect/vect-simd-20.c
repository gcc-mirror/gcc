/* { dg-additional-options "-fopenmp-simd -fno-tree-vectorize" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { target i?86-*-* x86_64-*-* } } } */

#include "tree-vect.h"

__attribute__((noipa)) int
foo (int s, int m, int n, int *p)
{
  int r = 0, l = 0, i, j;
  #pragma omp simd reduction (+:r) linear(l) collapse(2)
  for (j = 0; j < 7; j++)
    for (i = m; i < n; i += s)
      {
	p[l++] = i;
	r += i * 3;
      }
  return r;
}

int p[((10000 / 78) + 1) * 7];

int
main ()
{
  int i, j, r;
  check_vect ();
  r = foo (78, 0, 10000, p);
  for (j = 0; j < 7; j++)
#pragma GCC novector
    for (i = 0; i < 10000 / 78; i++)
      if (p[j * (10000 / 78 + 1) + i] != 78 * i)
	abort ();
  if (r != (10000 / 78) * (10000 / 78 + 1) / 2 * 78 * 3 * 7)
    abort ();
  r = foo (87, 0, 10000, p);
  for (j = 0; j < 7; j++)
#pragma GCC novector
    for (i = 0; i < 10000 / 87; i++)
      if (p[j * (10000 / 87 + 1) + i] != 87 * i)
	abort ();
  if (r != (10000 / 87) * (10000 / 87 + 1) / 2 * 87 * 3 * 7)
    abort ();
  return 0;
}
