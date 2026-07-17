/* { dg-do run } */
/* { dg-options "-O2 --param=max-rtl-if-conversion-unpredictable-cost=100 -fdump-rtl-ce1" } */

volatile long ga, gt, ge;

__attribute__ ((noipa)) void
convertible (long c, long p, long q)
{
  long a, t;
  if (c > 7)
    {
      a = p + 1;
      t = q + 2;
    }
  else
    {
      a = p - 3;
      t = q - 4;
    }
  ga = a;
  gt = t;
}

__attribute__ ((noipa)) void
reject_arm_only_values (long c, long p, long q, long t, long e)
{
  long a;
  if (c > 7)
    {
      a = p + 1;
      t = q + 2;
    }
  else
    {
      a = p - 3;
      e = q - 4;
    }
  ga = a;
  gt = t;
  ge = e;
}

__attribute__ ((optimize ("O0"))) int
main (void)
{
  convertible (8, 10, 20);
  if (ga != 11 || gt != 22)
    __builtin_abort ();
  convertible (7, 10, 20);
  if (ga != 7 || gt != 16)
    __builtin_abort ();

  reject_arm_only_values (8, 10, 20, 31, 47);
  if (ga != 11 || gt != 22 || ge != 47)
    __builtin_abort ();
  reject_arm_only_values (7, 10, 20, 31, 47);
  if (ga != 7 || gt != 31 || ge != 16)
    __builtin_abort ();
  return 0;
}

/* The first diamond is handled by the existing conditional-move path.  The
   second must be rejected because each arm has a live-out value not assigned
   by the other arm.  */
/* { dg-final { scan-rtl-dump-not "if-conversion succeeded through noce_convert_multiple_sets" "ce1" } } */
