/* { dg-additional-options "-mavx2" { target avx_runtime } } */

#include "tree-vect.h"

extern void abort (void);

struct X { double x; double y; };

void __attribute__((noinline,noclone))
foo (struct X *x, double px, int s)
{
  int i;
  for (i = 0; i < 256; ++i)
    {
      x[i*s].x = px;
      x[i*s].y = i + px;
    }
}

int main()
{
  struct X x[512];
  int i;
  check_vect ();
  foo (x, 1., 2);
  if (x[0].x != 1. || x[0].y != 1.
      || x[510].x != 1. || x[510].y != 256.)
    abort ();
  return 0;
}
