/* { dg-do compile } */
/* { dg-options "-O2 -fno-trapping-math -fsigned-zeros" } */

#include <math.h>

float
f1 (float x)
{
  return (int) rintf(x);
}

double
f2 (double x)
{
  return (long) rint(x);
}

/* { dg-final { scan-assembler "frintx\\ts\[0-9\]+, s\[0-9\]+" } } */
/* { dg-final { scan-assembler "cvtzs\\ts\[0-9\]+, s\[0-9\]+" } } */
/* { dg-final { scan-assembler "scvtf\\ts\[0-9\]+, s\[0-9\]+" } } */
/* { dg-final { scan-assembler "frintx\\td\[0-9\]+, d\[0-9\]+" } } */
/* { dg-final { scan-assembler "cvtzs\\td\[0-9\]+, d\[0-9\]+" } } */
/* { dg-final { scan-assembler "scvtf\\td\[0-9\]+, d\[0-9\]+" } } */

