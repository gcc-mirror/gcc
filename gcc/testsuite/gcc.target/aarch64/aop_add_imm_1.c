/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Disjoint constants that are ADD but not logical immediates should
   not need a separate move.  */

int
f (int x)
{
  return (x & ~0xfff) | 1000;
}

long
h (long x)
{
  return (x & ~0xfffL) | 0x123;
}

/* { dg-final { scan-assembler-times {\tadd\t[wx][0-9]+, [wx][0-9]+, \d+\n} 2 } } */
/* { dg-final { scan-assembler-not {\tmov\t} } } */
