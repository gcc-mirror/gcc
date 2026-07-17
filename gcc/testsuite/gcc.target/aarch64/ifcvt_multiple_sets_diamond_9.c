/* Do not extend dependency-aware conversion to register-copy permutations.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-ter -fno-tree-coalesce-vars -fdump-rtl-ce1" } */
/* { dg-additional-options "--param=max-rtl-if-conversion-unpredictable-cost=100" } */
/* { dg-additional-options "--param=max-rtl-if-conversion-predictable-cost=100" } */

volatile long gx, gy;

void
f (long c, long a, long b)
{
  long x, y;
  if (c > 7)
    {
      x = a;
      y = x;
      x = b;
    }
  else
    {
      x = b;
      y = x;
      x = a;
    }
  gx = x;
  gy = y;
}

/* { dg-final { scan-rtl-dump-not "if-conversion succeeded through noce_convert_multiple_sets" "ce1" } } */
/* { dg-final { scan-assembler {\tb(eq|ne|cs|cc|mi|pl|vs|vc|hi|ls|ge|lt|gt|le)\t} } } */
