/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mtune=generic --param=max-rtl-if-conversion-unpredictable-cost=0 -fdump-rtl-ce1" } */

/* These functions describe the same branch probabilities and arm costs with
   the arms reversed.  The scaled cost difference is exactly halfway between
   two integers.  */
long
then_cheaper (long c, long a, long b)
{
  long x;
  if (__builtin_expect_with_probability (c != 0, 0, 0.9375))
    x = a ^ b;
  else
    x = b * 3 + 1;
  return x;
}

long
then_costlier (long c, long a, long b)
{
  long x;
  if (__builtin_expect_with_probability (c == 0, 1, 0.9375))
    x = b * 3 + 1;
  else
    x = a ^ b;
  return x;
}

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_try_cmove_arith" 2 "ce1" } } */
/* { dg-final { scan-assembler-times {\tcmov} 2 } } */
/* { dg-final { scan-assembler-not {\tj(e|ne)\t} } } */
