/* { dg-do compile } */
/* { dg-options "-O2" } */

double
foo (int x)
{
  return (double) (float) ((x & -16) | (int) 0xf0000000);
}

/* { dg-final { scan-assembler {\tscvtf\td0, w[0-9]*} } } */
