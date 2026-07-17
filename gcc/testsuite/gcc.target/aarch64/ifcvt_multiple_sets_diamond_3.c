/* Test dependencies between sets in both arms of an IF-THEN-ELSE-JOIN
   diamond.  */
/* { dg-do run } */
/* { dg-options "-O2 --param=max-rtl-if-conversion-unpredictable-cost=100 -fdump-rtl-ce1" } */
/* Keep both assignments to x in the same RTL pseudo.  */
/* { dg-additional-options "-fno-tree-ter -fno-tree-coalesce-vars" } */

volatile long gx, gy;

__attribute__ ((noipa)) void
diamond_dependencies (long c, long a, long b)
{
  long x, y;
  if (c & 1)
    {
      x = a + 1;
      y = x ^ b;
      x = y + 3;
    }
  else
    {
      x = b - 1;
      y = x ^ a;
      x = y - 3;
    }
  gx = x;
  gy = y;
}

__attribute__ ((optimize ("O0"))) int
main (void)
{
  for (long c = -3; c <= 3; ++c)
    for (long a = -5; a <= 5; ++a)
      for (long b = -5; b <= 5; ++b)
	{
	  long x, y;
	  diamond_dependencies (c, a, b);
	  if (c & 1)
	    {
	      long first_x = a + 1;
	      y = first_x ^ b;
	      x = y + 3;
	    }
	  else
	    {
	      long first_x = b - 1;
	      y = first_x ^ a;
	      x = y - 3;
	    }
	  if (gx != x || gy != y)
	    __builtin_abort ();
	}
  return 0;
}

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_convert_multiple_sets" 1 "ce1" } } */
