/* { dg-do compile } */
/* { dg-options "-O2" } */

int foo()
{
  int t = -2147483648;
  int r = __builtin_bfin_abs_fr1x32(t);
  return r;
}

/* { dg-final { scan-assembler "32767" } } */
