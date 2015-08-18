/* { dg-do compile } */
/* { dg-options "-O2" } */

double
foo (unsigned int x)
{
  return (double) (float) (x & 0xffffff80);
}

/* { dg-final { scan-assembler {\tucvtf\ts[0-9]*, w[0-9]*} } } */
/* { dg-final { scan-assembler {\tfcvt\td0, s[0-9]*} } } */
