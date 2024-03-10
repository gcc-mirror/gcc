/* Check add and sub instructions (pseudoc asm dialect).  */
/* { dg-do compile } */
/* { dg-options "-masm=pseudoc" } */

long foo (long x, long y)
{
  return y - x + 4;
}

/* { dg-final { scan-assembler-not {\t(r.) -= \1\n} } } */
/* { dg-final { scan-assembler {\t(r.) -= (r.)\n} } } */
/* { dg-final { scan-assembler {\t(r.) \+= 4\n} } } */

