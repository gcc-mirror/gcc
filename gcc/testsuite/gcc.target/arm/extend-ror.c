/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-march=*" } { "-march=armv6" } } */
/* { dg-require-effective-target arm_arm_ok } */
/* { dg-add-options arm_arch_v6 } */
/* { dg-additional-options "-O -marm" } */

unsigned int zeroextractsi2_8_8(unsigned int x)
{
  return (unsigned char)(x>>8);
}

unsigned int zeroextractsi2_8_16(unsigned int x)
{
  return (unsigned char)(x>>16);
}

unsigned int signextractsi2_8_8(unsigned int x)
{
  return (int)(signed char)(x>>8);
}

unsigned int signextractsi2_8_16(unsigned int x)
{
  return (int)(signed char)(x>>16);
}

unsigned int zeroextractsi2_16_8(unsigned int x)
{
  return (unsigned short)(x>>8);
}

unsigned int signextractsi2_16_8(unsigned int x)
{
  return (int)(short)(x>>8);
}

/* { dg-final { scan-assembler-times ", ror #8" 4 } } */
/* { dg-final { scan-assembler-times ", ror #16" 2 } } */
