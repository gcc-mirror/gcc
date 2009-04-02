/* { dg-do compile { target powerpc*-*-* spu-*-* } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "and" } } */
/* There should be no nand for this testcase (for either PPC or SPU). */
/* xfail: PR tree-optimization/33512  */
/* { dg-final { scan-assembler-not "nand" { xfail *-*-* } } } */

int f(int y)
{
  return y & ~(y & -y);
}
