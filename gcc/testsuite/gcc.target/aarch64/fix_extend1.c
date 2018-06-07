/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned long
f7 (double x)
{
  return (unsigned) x;
}

unsigned long
f7_2 (float x)
{
  return (unsigned) x;
}

/* { dg-final { scan-assembler "fcvtzu\\tw\[0-9\]+, d\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtzu\\tw\[0-9\]+, s\[0-9\]+" } } */
