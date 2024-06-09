/* Check ISA V4 signed load instructions.  */
/* { dg-do compile } */
/* { dg-options "-mcpu=v4 -O2 -masm=normal" } */

long foo (char *p1, short *p2, int *p3)
{
  long x = *p1;
  long y = *p2;
  long z = *p3;

  return x + y + z;
}

/* { dg-final { scan-assembler {ldxsb\t%r.,\[%r.\+-?[0-9]+\]\n} } }  */
/* { dg-final { scan-assembler {ldxsh\t%r.,\[%r.\+-?[0-9]+\]\n} } }  */
/* { dg-final { scan-assembler {ldxsw\t%r.,\[%r.\+-?[0-9]+\]\n} } }  */
