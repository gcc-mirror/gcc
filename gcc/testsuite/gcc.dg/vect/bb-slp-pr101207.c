/* { dg-additional-options "-ffast-math" } */

#include "tree-vect.h"

double a[2];
double x, y;

void __attribute__((noipa)) foo ()
{
  x = a[1] - a[0];
  y = a[0] + a[1];
}

int main()
{
  check_vect ();

  a[0] = 0.;
  a[1] = 1.;
  foo ();
  if (x != 1. || y != 1.)
    __builtin_abort ();
  return 0;
}
