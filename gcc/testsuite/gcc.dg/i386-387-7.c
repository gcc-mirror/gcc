/* Verify that 387 fsincos instruction is generated.  */
/* { dg-do compile { target "i?86-*-*" } } */
/* { dg-options "-O -ffast-math -march=i686" } */
/* { dg-final { scan-assembler "fsincos" } } */

double f1(double x)
{
  return sin(x) + cos (x);
}

