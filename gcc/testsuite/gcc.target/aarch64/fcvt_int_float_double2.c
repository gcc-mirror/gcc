/* { dg-do compile } */
/* { dg-options "-O2" } */

double
foo (int x)
{
  return (double) (float) (x | (int) 0xfe000000);
}

/* { dg-final { scan-assembler {\tscvtf\ts[0-9]*, w[0-9]*} } } */
/* { dg-final { scan-assembler {\tfcvt\td0, s[0-9]*} } } */
