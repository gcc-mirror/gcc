/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -lm -fno-ident" } */

#include <math.h>

int
main (int argc, char *argv[])
{
  volatile double result;

  result = pow (-0.0, 3.0);
  result = pow (26.47, -2.0);
  result = pow (0.0, 0.0);
  result = pow (22.3, 1.0);
  result = pow (33.2, -1.0);

  return 0;
}


/* { dg-final { scan-assembler-not {pow\M} } } */
