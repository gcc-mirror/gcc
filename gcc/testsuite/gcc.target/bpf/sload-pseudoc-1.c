/* Check ISA V4 signed load instructions (pseudo-C dialect).  */
/* { dg-do compile } */
/* { dg-options "-mcpu=v4 -O2 -masm=pseudoc" } */

long foo (char *p1, short *p2, int *p3)
{
  long x = *p1;
  long y = *p2;
  long z = *p3;

  return x + y + z;
}

/* { dg-final { scan-assembler {r. = \*\(s8 \*\) \(r.\+-?[0-9]+\)\n} } } */
/* { dg-final { scan-assembler {r. = \*\(s16 \*\) \(r.\+-?[0-9]+\)\n} } } */
/* { dg-final { scan-assembler {r. = \*\(s32 \*\) \(r.\+-?[0-9]+\)\n} } } */
