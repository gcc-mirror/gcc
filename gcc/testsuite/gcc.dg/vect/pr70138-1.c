#include "tree-vect.h"

double u[33 * 33];

__attribute__((noinline, noclone)) static void
foo (int *x)
{
  double c = 0.0;
  int a, b;
  for (a = 0; a < 33; a++)
    {
      for (b = 0; b < 33; b++)
	c = c + u[34 * a];
      u[34 * a] *= 2.0;
    }
  *x = c;
}

int
main ()
{
  int d, e;
  check_vect ();
  for (d = 0; d < 33 * 33; d++)
    {
      u[d] = 499.0;
      __asm__ volatile ("" : : : "memory");
    }
  for (d = 0; d < 33; d++)
    {
      u[d * 34] = (d + 2);
      __asm__ volatile ("" : : : "memory");
    }
  foo (&e);
  if (e != 33 * (2 + 34) / 2 * 33)
    __builtin_abort ();
  return 0;
}
