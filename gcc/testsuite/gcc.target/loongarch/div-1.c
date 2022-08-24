/* { dg-do compile } */
/* { dg-options "-O2 -mcheck-zero-division" } */
/* { dg-final { scan-assembler "div.\[wd\]\t\\\$r4,\\\$r4,\\\$r5" } } */

long
div(long a, long b)
{
  return a / b;
}
