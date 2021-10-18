/* { dg-do compile } */
/* { dg-options "-O2" } */

short foo()
{
  short t = -32768;
  short r = __builtin_bfin_negate_fr1x16(t);
  return r;
}

/* { dg-final { scan-assembler "32767" } } */
