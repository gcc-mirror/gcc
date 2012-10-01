/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_arch_v6_ok } */
/* { dg-add-options arm_arch_v6 } */
/* { dg-final { scan-assembler-not "orr\[ \t\]" } } */

unsigned short swapu16_1 (unsigned short x)
{
  return (x << 8) | (x >> 8);
}

unsigned short swapu16_2 (unsigned short x)
{
  return (x >> 8) | (x << 8);
}
