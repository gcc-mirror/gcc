/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=olympus --param=max-rtl-if-conversion-unpredictable-cost=1 --param=max-rtl-if-conversion-predictable-cost=1 -fdump-rtl-ce1" } */

/* Outside loops, the target accepts the sequence before checking its cost.  */

volatile unsigned long output[2];

void
select_values (unsigned long c, unsigned long x, unsigned long y)
{
  unsigned long a, b;
  if (c > 7)
    {
      a = x + 9;
      a ^= y;
      a >>= 2;
      a += 5;
      a ^= y;
      a >>= 3;
      a += 13;
      a ^= y;
      b = 2;
    }
  else
    {
      a = x + 9;
      b = 3;
    }
  output[0] = a;
  output[1] = b;
}

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_convert_multiple_sets" 1 "ce1" } } */
