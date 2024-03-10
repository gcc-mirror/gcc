/* { dg-additional-options "-Ofast -fno-common" } */
/* { dg-additional-options "-Ofast -fno-common -mavx" { target avx_runtime } } */

#include "tree-vect.h"

__attribute__((noinline, noclone)) void
foo (double *x, double *y)
{
  double *p = __builtin_assume_aligned (x, 16);
  double *q = __builtin_assume_aligned (y, 16);
  double z, h;
  int i;
  for (i = 0; i < 1024; i++)
    {
      if (p[i] < 0.0)
	z = q[i], h = q[i] * 7.0 + 3.0;
      else
	z = p[i] + 6.0, h = p[1024 + i];
      p[i] = z + 2.0 * h;
    }
}

double a[2048] __attribute__((aligned (16)));
double b[1024] __attribute__((aligned (16)));

int
main ()
{
  int i;
  check_vect ();
  for (i = 0; i < 1024; i++)
    {
      a[i] = (i & 1) ? -i : 2 * i;
      a[i + 1024] = i;
      b[i] = 7 * i;
      asm ("");
    }
  foo (a, b);
#pragma GCC novector
  for (i = 0; i < 1024; i++)
    if (a[i] != ((i & 1)
		 ? 7 * i + 2.0 * (7 * i * 7.0 + 3.0)
		 : 2 * i + 6.0 + 2.0 * i)
	|| b[i] != 7 * i
	|| a[i + 1024] != i)
      abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target avx_runtime } } } */
