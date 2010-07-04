/* { dg-do compile } */
/* { dg-options "-mthumb -Os" }  */
/* { dg-require-effective-target arm_thumb1_ok } */

unsigned get_least_bits(unsigned value)
{
  return value << 9 >> 9;
}

/* { dg-final { scan-assembler "lsl" } } */
/* { dg-final { scan-assembler "lsr" } } */
