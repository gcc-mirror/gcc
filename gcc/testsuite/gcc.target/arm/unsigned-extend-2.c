/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-O" } */

unsigned short foo (unsigned short x)
{
  unsigned char i = 0;
  for (i = 0; i < 8; i++)
    {
      x >>= 1;
      x &= 0x7fff;
    }
  return x;
}

/* { dg-final { scan-assembler "ands" } } */
/* { dg-final { scan-assembler-not "uxtb" } } */
/* { dg-final { scan-assembler-not "cmp" } } */
