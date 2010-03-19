/* { dg-do compile } */
/* { dg-options "-Os -mthumb -march=armv5te" } */

unsigned get_least_bits(unsigned value)
{
  return value << 9 >> 9;
}

/* { dg-final { scan-assembler "lsl" } } */
/* { dg-final { scan-assembler "lsr" } } */
