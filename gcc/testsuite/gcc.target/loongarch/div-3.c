/* { dg-do compile } */
/* { dg-options "-O2 -mcheck-zero-division" } */
/* { dg-final { scan-assembler-not "div.\[wd\]\t\\\$r4,\\\$r5,\\\$r4" } } */

long
div(long a, long b)
{
  return b / a;
}
