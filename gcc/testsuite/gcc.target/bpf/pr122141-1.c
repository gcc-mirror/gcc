/* PR122141 miscompilation when j(s)l[te] unavailable.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=v1 -masm=normal" } */

int
f(long a, long b)
{
  return a > b ? 2 : 3;
}

/* { dg-final { scan-assembler "jsge\t%r2,%r1" } } */
