#include "tree-vect.h"

double x[2], y[2], z[2], w[2];

void __attribute__((noipa)) foo ()
{
  double tem0 = x[1] + y[1];
  double tem1 = x[0] - y[0];
  double tem2 = z[1] * tem0;
  double tem3 = z[0] * tem1;
  z[0] = tem2 - w[1];
  z[1] = tem3 + w[0];
}

int main()
{
  check_vect ();

  x[0] = 1.; x[1] = 2.;
  y[0] = 7.; y[1] = -5.;
  z[0] = 2.; z[1] = 3.;
  w[0] = 9.; w[1] = -5.;
  foo ();
  if (z[0] != -4. || z[1] != -3.)
    __builtin_abort ();
  return 0;
}
