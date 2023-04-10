/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

/* Verify peephole fires to combine div/mod using same opnds. */

long foo (long a, long b)
{
  long x, y;

  x = a / b;
  y = a % b;
  return (x + y);
}

unsigned long foo2 (unsigned long a, unsigned long b)
{
  unsigned long x, y;

  x = a / b;
  y = a % b;
  return (x + y);
}

/* { dg-final { scan-assembler-not {\mmodsd\M} } } */
/* { dg-final { scan-assembler-not {\mmodud\M} } } */
