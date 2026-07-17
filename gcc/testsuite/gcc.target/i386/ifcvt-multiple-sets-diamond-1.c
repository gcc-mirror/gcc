/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mtune=generic -fdump-rtl-ce1" } */
/* { dg-additional-options "--param=max-rtl-if-conversion-predictable-cost=100" } */

/* The single-set arm is the likely fallthrough block.  Speculative arithmetic
   clobbers FLAGS, so each conditional move must re-materialize the
   comparison.  */

volatile long ga, gb;

__attribute__ ((noipa)) void
f (long c, long x, long y, long b)
{
  long a;
  if (__builtin_expect (c <= 7, 1))
    a = y + 3;
  else
    {
      a = x + 1;
      b = y + 2;
    }
  ga = a;
  gb = b;
}

/* Keep the runtime driver out of noce so that the dump count is specific to
   F.  */
__attribute__ ((optimize ("O0"))) int
main (void)
{
  for (long c = 5; c != 11; ++c)
    for (long x = -8; x != 9; ++x)
      for (long y = -8; y != 9; ++y)
	{
	  f (c, x, y, 4);
	  if (ga != (c > 7 ? x + 1 : y + 3)
	      || gb != (c > 7 ? y + 2 : 4))
	    __builtin_abort ();
	}
  return 0;
}

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_convert_multiple_sets" 1 "ce1" } } */
