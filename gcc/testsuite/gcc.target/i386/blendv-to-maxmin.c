/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -O2 -mfpmath=sse" } */
/* { dg-final { scan-assembler-times "vmaxsd" 1 } } */

double
foo (double a)
{
  if (a > 0.0)
    return a;
  return 0.0;
}

