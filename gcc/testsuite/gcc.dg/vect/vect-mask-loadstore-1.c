/* { dg-additional-options "-Ofast -fno-common" } */
/* { dg-additional-options "-Ofast -fno-common -mavx" { target avx_runtime } } */

#include "tree-vect.h"

__attribute__((noinline, noclone)) void
foo (float *__restrict x, float *__restrict y, float *__restrict z)
{
  float *__restrict p = __builtin_assume_aligned (x, 32);
  float *__restrict q = __builtin_assume_aligned (y, 32);
  float *__restrict r = __builtin_assume_aligned (z, 32);
  int i;
  for (i = 0; i < 1024; i++)
    {
      if (p[i] < 0.0f)
	q[i] = p[i] + 2.0f;
      else
	p[i] = r[i] + 3.0f;
    }
}

float a[1024] __attribute__((aligned (32)));
float b[1024] __attribute__((aligned (32)));
float c[1024] __attribute__((aligned (32)));

int
main ()
{
  int i;
  check_vect ();
  for (i = 0; i < 1024; i++)
    {
      a[i] = (i & 1) ? -i : i;
      b[i] = 7 * i;
      c[i] = a[i] - 3.0f;
      asm ("");
    }
  foo (a, b, c);
  for (i = 0; i < 1024; i++)
    if (a[i] != ((i & 1) ? -i : i)
	|| b[i] != ((i & 1) ? a[i] + 2.0f : 7 * i)
	|| c[i] != a[i] - 3.0f)
      abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "note: vectorized 1 loops" 1 "vect" { target avx_runtime } } } */
