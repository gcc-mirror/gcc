/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb2_ok_no_arm_v8_1_lob } */
/* { dg-options "-O" } */

unsigned short foo (unsigned short x, unsigned short c)
{
  unsigned char i = 0;
  for (i = 0; i < 8; i++)
    {
      x >>= 1;
      x &= c;
    }
  return x;
}

/* { dg-final { scan-assembler "ands" } } */
/* { dg-final { scan-assembler-not "uxtb" } } */
/* { dg-final { scan-assembler-not "cmp" } } */
