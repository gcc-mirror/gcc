/* { dg-do compile } */
/* { dg-options "-O2" } */

double
foo (unsigned int x)
{
  return (double) (float) (x & 0x1ffffff);
}

/* { dg-final { scan-assembler {\t[su]cvtf\ts[0-9]*, w[0-9]*} } } */
/* { dg-final { scan-assembler {\tfcvt\td0, s[0-9]*} } } */
