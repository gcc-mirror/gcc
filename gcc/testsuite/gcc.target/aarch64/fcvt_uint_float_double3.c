/* { dg-do compile } */
/* { dg-options "-O2" } */

double
foo (unsigned int x)
{
  return (double) (float) (x & 0xffffff00);
}

/* { dg-final { scan-assembler {\tucvtf\td0, w[0-9]*} } } */
