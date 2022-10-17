/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

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

float
f3 (double x)
{
  int y = x;

  return (float) y;
}

double
f4 (float x)
{
  int y = x;

  return (double) y;
}

/* { dg-final { scan-assembler "frintz\\ts\[0-9\]+, s\[0-9\]+" } } */
/* { dg-final { scan-assembler "frintz\\td\[0-9\]+, d\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtzs\\tw\[0-9\]+, d\[0-9\]+" } } */
/* { dg-final { scan-assembler "scvtf\\ts\[0-9\]+, w\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtzs\\tw\[0-9\]+, s\[0-9\]+" } } */
/* { dg-final { scan-assembler "scvtf\\td\[0-9\]+, w\[0-9\]+" } } */
