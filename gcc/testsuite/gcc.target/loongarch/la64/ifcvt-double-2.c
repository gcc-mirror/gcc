/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-ce1 -mtune=la664" } */
/* { dg-final { scan-rtl-dump "3 true changes made" "ce1" } } */
/* { dg-final { scan-assembler "movgr2cf\t\\\$fcc0,\\\$r4" } } */

double
foo (double a, double b, int c)
{
  return c > 0 ? a : b;
}
