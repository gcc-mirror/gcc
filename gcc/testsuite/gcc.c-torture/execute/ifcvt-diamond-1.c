/* Runtime correctness of if-converted IF-THEN-ELSE-JOIN diamonds with
   multiple output registers (noce_convert_multiple_sets).  */

long g1, g2, g3;

__attribute__ ((noipa)) void
diamond2 (long c, long x, long y)
{
  long a, b;
  if (c & 3)
    {
      a = x + 1;
      b = y - 2;
    }
  else
    {
      a = x * 4;
      b = y + 9;
    }
  g1 = a;
  g2 = b;
}

/* Then arm reads an earlier then output (arm-internal dependency).  */
__attribute__ ((noipa)) void
diamond3 (long c, long p, long q)
{
  long a, b, d;
  if (c > 0)
    {
      a = p ^ q;
      b = a + 7;
      d = q * 2;
    }
  else
    {
      a = p & q;
      b = q | 1;
      d = p - 3;
    }
  g1 = a;
  g2 = b;
  g3 = d;
}

/* A single output in the else arm.  The other register keeps its incoming
   value on the else path.  */
__attribute__ ((noipa)) void
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
  g1 = a;
  g2 = b;
}

/* Keep the single-set arm as the likely fallthrough block.  The multi-set
   arm must become the primary arm of the conversion.  */
__attribute__ ((noipa)) void
diamond_reversed_then2_else1 (long c, long x)
{
  long type = c & 3;
  long next = type;
  long advance;
  if (__builtin_expect (type != 0, 1))
    advance = type + 1;
  else
    {
      next = x + 1;
      advance = x + 2;
    }
  g1 = next;
  g2 = advance;
}

/* Each arm produces a live-out value that the other arm does not.  */
__attribute__ ((noipa)) void
diamond_unmatched_liveouts (long c, long p, long q)
{
  long a = 100, t = 300, e = 200;
  if (c & 4)
    {
      a = p + q;
      t = p * 2;
    }
  else
    {
      a = p - q;
      e = q * 2;
    }
  g1 = a;
  g2 = e;
  g3 = t;
}

int
main (void)
{
  for (long c = -4; c <= 12; c++)
    for (long p = -6; p <= 6; p++)
      for (long q = -6; q <= 6; q++)
	{
	  diamond2 (c, p, q);
	  if (g1 != ((c & 3) ? p + 1 : p * 4)
	      || g2 != ((c & 3) ? q - 2 : q + 9))
	    __builtin_abort ();

	  diamond3 (c, p, q);
	  {
	    long ea = (c > 0) ? (p ^ q) : (p & q);
	    long eb = (c > 0) ? ea + 7 : (q | 1);
	    long ed = (c > 0) ? (q * 2) : (p - 3);
	    if (g1 != ea || g2 != eb || g3 != ed)
	      __builtin_abort ();
	  }

	  diamond_then2_else1 (c, p, q);
	  if (g1 != ((c < 0) ? p + 100 : p - 50)
	      || g2 != ((c < 0) ? q + 200 : q))
	    __builtin_abort ();

	  diamond_reversed_then2_else1 (c, p);
	  {
	    long type = c & 3;
	    if (g1 != (type ? type : p + 1)
		|| g2 != (type ? type + 1 : p + 2))
	      __builtin_abort ();
	  }

	  diamond_unmatched_liveouts (c, p, q);
	  if (g1 != ((c & 4) ? p + q : p - q)
	      || g2 != ((c & 4) ? 200 : q * 2)
	      || g3 != ((c & 4) ? p * 2 : 300))
	    __builtin_abort ();
	}
  return 0;
}
