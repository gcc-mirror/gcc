/* { dg-additional-options "-O3" } */

#include "tree-vect.h"

short a, b, f, g;
int c = 4, d, e = -1L;
long h = 4;

int
main ()
{
  check_vect ();

  long i;
  for (; d <= 55; d++)
    {
      g = c >= 2 ? 0 : b << c;
      f = g - a;
      i = (f ^ 9223372036854775807) < 0 ? f : h;
      e &= i;
    }
  if (e != 4)
    __builtin_abort ();

  return 0;
}
