/* { dg-do compile } */
/* { dg-options "-O2 -march=k8" } */
/* { dg-final { scan-assembler "cmov\[^3\]" } } */

/* This conditional move is fastest to be done using cmov.  */
t(int a, int b)
{
  return (a<=b?5:-5);
}
