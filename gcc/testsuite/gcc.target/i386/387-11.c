/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -mfpmath=387" } */

double foo(double x, double y)
{
  double t = -x * y;
  return -t;
}

/* { dg-final { scan-assembler-not "fchs" } } */
