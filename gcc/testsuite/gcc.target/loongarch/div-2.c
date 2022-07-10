/* { dg-do compile } */
/* { dg-options "-O2 -mno-check-zero-division" } */
/* { dg-final { scan-assembler "div.\[wd\]\t\\\$r4,\\\$r5,\\\$r4" } } */

long
div(long a, long b)
{
  return b / a;
}
