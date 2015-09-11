/* { dg-do compile } */
/* { dg-options "-O2" } */

double
foo (unsigned int x)
{
  return (double) (float) (x & 0xffffff);
}

/* { dg-final { scan-assembler {\t[su]cvtf\td0, w[0-9]*} } } */
