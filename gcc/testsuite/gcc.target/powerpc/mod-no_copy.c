/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

/* Verify r3 is used as source and target, no copy inserted. */

long foo (long a, long b)
{
  return (a % b);
}

unsigned long foo2 (unsigned long a, unsigned long b)
{
  return (a % b);
}

/* { dg-final { scan-assembler-not {\mmr\M} } } */
