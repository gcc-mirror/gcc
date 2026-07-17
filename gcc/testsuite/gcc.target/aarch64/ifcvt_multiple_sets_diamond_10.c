/* Use dependency-aware conversion when one arm forwards the live-outs and
   the other computes them.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-ssa-phiopt -fno-tree-ter -fno-tree-coalesce-vars -fdump-rtl-ce1" } */
/* { dg-additional-options "--param=max-rtl-if-conversion-unpredictable-cost=100 --param=max-rtl-if-conversion-predictable-cost=100" } */

void sink2 (long, long);

void
mixed_forwarded_live_outs (long c, long count, long first, long step)
{
  if (c > 7)
    {
      first = count;
      count = step;
    }
  else
    {
      first = first + step + 1;
      count = count - step - 1;
    }
  sink2 (count, first);
}

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_convert_multiple_sets" 1 "ce1" } } */
/* { dg-final { scan-assembler-not {\tb(eq|ne|cs|cc|mi|pl|vs|vc|hi|ls|ge|lt|gt|le)\t} } } */
/* { dg-final { scan-assembler-not "\tcbn?z\t" } } */
/* { dg-final { scan-assembler-not "\ttbn?z\t" } } */
