/* { dg-do run } */
/* { dg-options "-ffast-math" } */

#include <math.h>

extern void abort (void);

double foo(void)
{
  double x = -4.0;
  return pow (x * x, 0.25);
}

int main()
{
  if (foo() != 2.0)
    abort ();
  return 0;
}
