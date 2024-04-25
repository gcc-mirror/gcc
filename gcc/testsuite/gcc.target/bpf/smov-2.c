/* Check signed 32-bit mov instructions.  */
/* { dg-do compile } */
/* { dg-options "-mcpu=v4 -O2 -masm=normal" } */

int
foo (unsigned char a, unsigned short b)
{
  int x = (char) a;
  int y = (short) b;

  return x + y;
}

/* { dg-final { scan-assembler {movs32\t%r.,%r.,8\n} } } */
/* { dg-final { scan-assembler {movs32\t%r.,%r.,16\n} } } */
