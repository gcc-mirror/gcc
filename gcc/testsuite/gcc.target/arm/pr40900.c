/* { dg-do compile } */
/* { dg-options "-O2 -fno-optimize-sibling-calls" }  */

extern short shortv2();
short shortv1()
{
  return shortv2();
}

/* { dg-final { scan-assembler-not "lsl" } } */
/* { dg-final { scan-assembler-not "asr" } } */
/* { dg-final { scan-assembler-not "sxth" } } */
