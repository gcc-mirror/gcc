/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -march=k8" } */
/* { dg-final { scan-assembler "cmov" } } */

/* This conditional move is fastest to be done using cmov.  */
t(int a, int b)
{
  return (a<=b?5:-5);
}
