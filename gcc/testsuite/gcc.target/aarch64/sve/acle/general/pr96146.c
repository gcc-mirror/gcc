/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O3" } */

#include <arm_sve.h>

void __attribute__ ((noipa))
f (volatile int *x)
{
  int i;
  for (int i = 0; i < svcntd (); ++i)
    *x = i;
}

int
main (void)
{
  volatile int x;
  f (&x);
  return 0;
}
