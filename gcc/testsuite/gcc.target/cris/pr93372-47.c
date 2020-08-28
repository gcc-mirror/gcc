/* { dg-do compile } */
/* { dg-options "-O2 -march=v10" } */
/* { dg-final { scan-assembler-times {\tnop} 1 } } */

/* A somewhat brittle test-case, checking that we have (only) one
   unfilled delay-slot in random_bitstring: there might be none or two
   or more, and general improvements may lead to unfilled delay-slots.
   When the scan-assembler-times directive regresses, re-run
   gcc.c-torture/execute/arith-rand-ll.c, check cycle-level
   execution-time regressions in random_bitstring and take appropriate
   action.  */

static long long
simple_rand ()
{
  static unsigned long long seed = 47114711;
  unsigned long long this = seed * 1103515245 + 12345;
  seed = this;
  return this >> 8;
}

unsigned long long int
random_bitstring ()
{
  unsigned long long int x;
  int n_bits;
  long long ran;
  int tot_bits = 0;

  x = 0;
  for (;;)
    {
      ran = simple_rand ();
      n_bits = (ran >> 1) % 16;
      tot_bits += n_bits;

      if (n_bits == 0)
	return x;
      else
	{
	  x <<= n_bits;
	  if (ran & 1)
	    x |= (1 << n_bits) - 1;

	  if (tot_bits > 8 * sizeof (long long) + 6)
	    return x;
	}
    }
}
