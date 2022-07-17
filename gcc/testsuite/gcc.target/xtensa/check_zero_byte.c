/* { dg-do compile } */
/* { dg-options "-O" } */

int check_zero_byte(int v)
{
  return (v - 0x01010101) & ~v & 0x80808080;
}

/* { dg-final { scan-assembler-not "movi" } } */
