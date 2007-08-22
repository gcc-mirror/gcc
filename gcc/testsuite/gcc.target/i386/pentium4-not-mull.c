/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -march=pentium4" } */
/* { dg-final { scan-assembler-not "imull" } } */

/* Should be done not using imull.  */
int t(int x)
{
  return x*29;
}
