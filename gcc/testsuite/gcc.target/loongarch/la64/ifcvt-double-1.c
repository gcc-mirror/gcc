/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-ce1" } */
/* { dg-final { scan-rtl-dump "3 true changes made" "ce1" } } */
/* { dg-final { scan-assembler "movgr2fr\\.w\t\\\$f\[0-9\]+,\\\$r4" } } */
/* { dg-final { scan-assembler "movfr2cf\t\\\$fcc0" } } */

double
foo (double a, double b, int c)
{
  return c > 0 ? a : b;
}
