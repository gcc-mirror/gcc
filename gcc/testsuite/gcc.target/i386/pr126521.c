/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -fno-signed-zeros -fno-ssa-phiopt" } */

double g (double a)
{
  double x;
  if (a < 0.0)
    x = a;
  else
    x = -a;
  return x;
}

/* { dg-final { scan-assembler "cmpnlt" } } */
