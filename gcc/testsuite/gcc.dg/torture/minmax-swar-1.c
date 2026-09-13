/* { dg-do run } */
/* { dg-require-effective-target int32 } */

/* The canonicalization must not change any value.  Every difference formed
   below stays inside the type, so the test is also valid under -ftrapv (where
   the fold itself is disabled, the branchless form being the one that traps).  */

static int __attribute__ ((noipa))
swar_min (int a, int b)
{
  int t = a - b;
  return b + (t & (t >> 31));
}

static int __attribute__ ((noipa))
swar_max (int a, int b)
{
  int t = a - b;
  return a - (t & (t >> 31));
}

static int __attribute__ ((noipa))
swar_median (int a, int b, int c)
{
  int t = (a - b) & ((a - b) >> 31);
  a -= t;
  b += t;
  b -= (b - c) & ((b - c) >> 31);
  b += (a - b) & ((a - b) >> 31);
  return b;
}

static int __attribute__ ((noipa))
clamp_lo (int x, int y)
{
  int s = x + y;
  return (s < y ? s : y) - y;
}

static int __attribute__ ((noipa))
clamp_hi (int x, int y)
{
  int s = x + y;
  return (s > y ? s : y) - y;
}

static int __attribute__ ((noipa))
ref_median (int a, int b, int c)
{
  int lo = a < b ? (a < c ? a : c) : (b < c ? b : c);
  int hi = a > b ? (a > c ? a : c) : (b > c ? b : c);
  return a + b + c - lo - hi;
}

int
main (void)
{
  static const int vs[] = { -268435456, -65536, -257, -1, 0, 1, 257, 65536,
			    268435455 };
  unsigned n = sizeof (vs) / sizeof (vs[0]);
  for (unsigned i = 0; i < n; i++)
    for (unsigned j = 0; j < n; j++)
      {
	int a = vs[i], b = vs[j];
	if (swar_min (a, b) != (a < b ? a : b))
	  __builtin_abort ();
	if (swar_max (a, b) != (a > b ? a : b))
	  __builtin_abort ();
	if (a < 268435456 && a > -268435456 && b < 268435456 && b > -268435456)
	  {
	    if (clamp_lo (a, b) != (a < 0 ? a : 0))
	      __builtin_abort ();
	    if (clamp_hi (a, b) != (a > 0 ? a : 0))
	      __builtin_abort ();
	  }
	for (unsigned k = 0; k < n; k++)
	  if (swar_median (a, b, vs[k]) != ref_median (a, b, vs[k]))
	    __builtin_abort ();
      }
  return 0;
}
