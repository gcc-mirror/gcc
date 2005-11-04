/* Test argument passing with SSE2 and local functions
   Written by Paolo Bonzini, 25 January 2005 */

/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-final { scan-assembler "movss" } } */
/* { dg-final { scan-assembler "mulss" } } */
/* { dg-final { scan-assembler "movsd" } } */
/* { dg-final { scan-assembler "mulsd" } } */
/* { dg-options "-O2 -msse2 -mfpmath=sse -fno-inline" } */

static float xs (void)
{
  return 3.14159265;
}

float ys (float a)
{
  return xs () * a;
}

static double xd (void)
{
  return 3.1415926535;
}

double yd (double a)
{
  return xd () * a;
}

