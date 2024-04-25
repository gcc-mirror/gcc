/* Check negr and negr32 instructions.  */

/* { dg-do compile } */
/* { dg-options "-malu32 -masm=normal" } */

long foo (long a, long b, int x, int y)
{
  a = -b;
  x = -y;
  return a + x;
}

/* { dg-final { scan-assembler "neg\t%r.\n" } } */
/* { dg-final { scan-assembler "neg32\t%r.\n" } } */
