/* Test if-conversion of IF-THEN-ELSE-JOIN diamonds with multiple output
   registers through noce_convert_multiple_sets.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-ce1" } */
/* { dg-additional-options "--param=max-rtl-if-conversion-unpredictable-cost=100" } */
/* { dg-additional-options "--param=max-rtl-if-conversion-predictable-cost=100" } */

void sink2 (long, long);

/* Two outputs, both arms write the same registers.  */
void
diamond_arith (long c, long x, long y)
{
  long a, b;
  if (c > 7)
    {
      a = x + 1;
      b = y - 2;
    }
  else
    {
      a = x * 4;
      b = y + 9;
    }
  sink2 (a, b);
}

/* Two outputs computed from constants on each arm.  */
void
diamond_const (long c, long x, long y)
{
  long a, b;
  if (c == 3)
    {
      a = 5;
      b = 7;
    }
  else
    {
      a = 9;
      b = 11;
    }
  sink2 (a, b);
}

/* Two outputs in the then arm, a single output in the else arm.  The second
   register keeps its incoming value on the else path.  */
void
diamond_then2_else1 (long c, long x, long y)
{
  long a = x, b = y;
  if (c < 0)
    {
      a = x + 100;
      b = y + 200;
    }
  else
    a = x - 50;
  sink2 (a, b);
}

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_convert_multiple_sets" 2 "ce1" } } */

/* The converted diamonds are branchless: no conditional branch remains.  */
/* { dg-final { scan-assembler-not {\tb(eq|ne|cs|cc|mi|pl|vs|vc|hi|ls|ge|lt|gt|le)\t} } } */
/* { dg-final { scan-assembler-not "\tcbn?z\t" } } */
/* { dg-final { scan-assembler-not "\ttbn?z\t" } } */
/* { dg-final { scan-assembler "\tcsel\t" } } */
