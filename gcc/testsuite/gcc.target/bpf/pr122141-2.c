/* PR122141 miscompilation when j(s)l[te] unavailable.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=v1 -masm=normal" } */

int
g(unsigned long a, unsigned long b)
{
  return a >= b ? 4 : 5;
}

/* { dg-final { scan-assembler "jgt\t%r2,%r1" } } */
