/* { dg-do compile } */
/* { dg-options "-O2" } */

float
f1 (float x)
{
  int y = x;

  return (float) y;
}

double
f2 (double x)
{
  long y = x;

  return (double) y;
}

/* { dg-final { scan-assembler "fcvtzs\\ts\[0-9\]+, s\[0-9\]+" } } */
/* { dg-final { scan-assembler "scvtf\\ts\[0-9\]+, s\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtzs\\td\[0-9\]+, d\[0-9\]+" } } */
/* { dg-final { scan-assembler "scvtf\\td\[0-9\]+, d\[0-9\]+" } } */
