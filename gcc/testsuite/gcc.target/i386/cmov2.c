/* { dg-do compile } */
/* { dg-options "-O2 -march=k8" } */
/* { dg-final { scan-assembler "sbb" } } */

/* This conditional move is fastest to be done using sbb.  */
int
t(unsigned int a, unsigned int b)
{
  return (a<=b?5:-5);
}
