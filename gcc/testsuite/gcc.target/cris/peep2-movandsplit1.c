/* { dg-do compile } */
/* { dg-final { scan-assembler-times "lsrq " 2 } } */
/* { dg-final { scan-assembler-times "lslq " 2 } } */
/* { dg-final { scan-assembler-times "move.d \\\$r11,\\\$r10" 2 } } */
/* { dg-final { scan-assembler-times "\tmov" 2 } } */
/* { dg-final { scan-assembler-not "\tand" } } */
/* { dg-options "-O2" } */

unsigned int xmovandr (unsigned int y, unsigned int x)
{
  return x & 0x7ffff;
}

unsigned int xmovandl (unsigned int y, unsigned int x)
{
  return x & 0xfffe0000;
}
