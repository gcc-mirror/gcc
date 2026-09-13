/* { dg-do run } */
/* { dg-additional-options "-fwrapv" } */
/* { dg-require-effective-target int32 } */

extern void abort (void);

static int __attribute__((noipa))
ref32 (int x, int c1, int c2)
{
  unsigned int u = (unsigned int) x + (unsigned int) c1;
  unsigned int v = (unsigned int) c2 - (unsigned int) x;
  return (u | v) >> 31;
}

static int __attribute__((noipa))
ref16 (int x, int c1, int c2)
{
  unsigned short u = (unsigned short) (x + c1);
  unsigned short v = (unsigned short) (c2 - x);
  return ((u | v) >> 15) & 1;
}

static int __attribute__((noipa)) t1 (int x) { return ((x + 1) | (1 - x)) < 0; }
static int __attribute__((noipa)) t2 (int x) { return ((x + 5) | (5 - x)) >= 0; }
static int __attribute__((noipa)) t3 (int x) { return (((x + 1) | (1 - x)) >> 31) & 1; }
static int __attribute__((noipa)) t4 (int x) { return ((x - 3) | (9 - x)) < 0; }
static int __attribute__((noipa)) t5 (unsigned int x) { return (int) ((x - 3) | (9 - x)) < 0; }
static int __attribute__((noipa)) t6 (short x) { return (short) ((x - 3) | (9 - x)) < 0; }

int
main (void)
{
  static const int v[] = { 0, 1, -1, 2, -2, 3, -3, 5, -5, 6, -6, 9, 10, -10,
			   1000, -1000, 32766, 32767, -32767, -32768,
			   2147483647, -2147483647 - 1, 2147483646,
			   -2147483647 };
  unsigned int i;

  for (i = 0; i < sizeof (v) / sizeof (v[0]); i++)
    {
      int x = v[i];

      if (t1 (x) != ref32 (x, 1, 1))
	abort ();
      if (t2 (x) == ref32 (x, 5, 5))
	abort ();
      if (t3 (x) != ref32 (x, 1, 1))
	abort ();
      if (t4 (x) != ref32 (x, -3, 9))
	abort ();
      if (t5 ((unsigned int) x) != ref32 (x, -3, 9))
	abort ();
      if (t6 ((short) x) != ref16 ((short) x, -3, 9))
	abort ();
    }
  return 0;
}
