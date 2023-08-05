/* Check signed mov instructions (pseudo-C asm dialect).  */
/* { dg-do compile } */
/* { dg-options "-mcpu=v4 -O2 -masm=pseudoc" } */

long
foo (char a, short b, int c, unsigned long d)
{
  long x = a;
  long y = b;
  long z = c;
  long w = (long) d;

  return x + y + z + w;
}

/* { dg-final { scan-assembler {r. = \(s8\) r.\n} } } */
/* { dg-final { scan-assembler {r. = \(s16\) r.\n} } } */
/* { dg-final { scan-assembler {r. = \(s32\) r.\n} } } */
