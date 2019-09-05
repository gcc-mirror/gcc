/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "and" { target powerpc*-*-* } } } */
/* There should be no nand for this testcase (for PPC). */
/* { dg-final { scan-assembler-not "nand" { target powerpc*-*-* } } } */

int f(int y)
{
  return y & ~(y & -y);
}
