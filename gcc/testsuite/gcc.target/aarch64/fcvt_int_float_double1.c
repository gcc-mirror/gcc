/* { dg-do compile } */
/* { dg-options "-O2" } */

double
foo (int x)
{
  return (double) (float) (x | (int) 0xff000000);
}

/* { dg-final { scan-assembler {\tscvtf\td0, w[0-9]*} } } */
