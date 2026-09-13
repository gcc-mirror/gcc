/* { dg-do run } */
/* { dg-require-effective-target int32 } */

extern void abort (void);

static int __attribute__((noipa))
ref (int x, int c)
{
  long long l = x;
  return (l < 0 ? -l : l) > c;
}

static int __attribute__((noipa)) a1 (int x) { return __builtin_abs (x) > 5; }
static int __attribute__((noipa)) a2 (int x) { return __builtin_abs (x) >= 6; }
static int __attribute__((noipa)) a3 (int x) { return __builtin_abs (x) > 0; }
static int __attribute__((noipa)) a4 (int x) { return __builtin_abs (x) > 0x3fffffff; }
static int __attribute__((noipa)) a5 (int x) { return __builtin_abs (x) <= 5; }
static int __attribute__((noipa)) a6 (int x) { return __builtin_abs (x) < 6; }

int
main (void)
{
  static const int v[] = { 0, 1, -1, 5, -5, 6, -6, 1000, -1000,
			   0x3fffffff, -0x3fffffff, 0x40000000, -0x40000000,
			   2147483647, -2147483647 };
  unsigned int i;

  for (i = 0; i < sizeof (v) / sizeof (v[0]); i++)
    {
      int x = v[i];

      if (a1 (x) != ref (x, 5) || a2 (x) != ref (x, 5)
	  || a3 (x) != ref (x, 0) || a4 (x) != ref (x, 0x3fffffff)
	  || a5 (x) == ref (x, 5) || a6 (x) == ref (x, 5))
	abort ();
    }
  return 0;
}
