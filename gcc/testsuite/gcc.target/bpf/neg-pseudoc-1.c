/* Check negr and negr32 instructions (pseudoc asm dialect.)  */

/* { dg-do compile } */
/* { dg-options "-malu32 -masm=pseudoc" } */

long foo (long a, long b, int x, int y)
{
  a = -b;
  x = -y;
  return a + x;
}

/* { dg-final { scan-assembler {\t(r.) = -\1\n} } } */
/* { dg-final { scan-assembler {\t(w.) = -\1\n} } } */
