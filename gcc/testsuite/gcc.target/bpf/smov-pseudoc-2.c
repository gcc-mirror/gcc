/* Check signed 32-bit mov instructions (pseudo-C asm dialect).  */
/* { dg-do compile } */
/* { dg-options "-mcpu=v4 -O2 -masm=pseudoc" } */

int
foo (unsigned char a, unsigned short b)
{
  int x = (char) a;
  int y = (short) b;

  return x + y;
}

/* { dg-final { scan-assembler {w. = \(s8\) w.\n} } } */
/* { dg-final { scan-assembler {w. = \(s16\) w.\n} } } */
