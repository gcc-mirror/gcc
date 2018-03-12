/* { dg-do run } */

#include "tree-vect.h"

#define N 256

void __attribute__ ((noinline, noclone))
f (unsigned long incx, unsigned long incy,
   float *restrict dx, float *restrict dy)
{
  unsigned long ix = 0, iy = 0;
  for (unsigned long i = 0; i < N; ++i)
    {
      dy[iy] += dx[ix];
      ix += incx;
      iy += incy;
    }
}

float a = 0.0;
float b[N];

int
main (void)
{
  check_vect ();

  for (int i = 0; i < N; ++i)
    b[i] = i;
  f (1, 0, b, &a);
  if (a != N * (N - 1) / 2)
    __builtin_abort ();
  return 0;
}
