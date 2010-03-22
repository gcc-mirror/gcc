/* { dg-do run } */
/* { dg-options "-O1" } */
/* { dg-options "-mieee -O1" { target alpha*-*-* sh*-*-* } } */
#include <math.h>

extern void abort (void);
void __attribute__((noinline)) f (double x)
{
  double pluszero = pow (x, 0.5);
  double minuszero = sqrt (x);
  if (signbit (pluszero) == signbit (minuszero))
    abort ();
}

int main(void)
{
  f (-0.0);
  return 0;
}
