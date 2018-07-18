/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

unsigned a, e;

struct S0
{
  int f1;
  int f8;
} c = {4, 6};

int b, f, g, h, i, j, l, p;
short d, o = -7;
char k, n = 5;

unsigned fn1 (int p1, int p2)
{
  return p2 >= 2 || p1 >> p2 ? p1 : p1 << p2;
}

static short fn2 (struct S0 p1)
{
  int *q = 0;
  int r = 7;
  if (!a)
    {
      c.f1 = 1;
      for (; c.f1; c.f1--)
	if (fn1 (10 != 0, p1.f8))
	  {
	    short s = 9 << ~o % (d ^ n);
	    int t = s >> h % ~d;
	    p = r;
	    r = s | p * (d && 9) | t;
	    int u = i & c.f1;
	    unsigned v = ~(~(u & h) | (~(8 >> c.f1) & i));
	    int w = v;
	    if (u < 9)
	      w = c.f1;
	    if (i > h && u)
	      {
		__builtin_printf ("%d\n", c.f1);
		continue;
	      }
	    c.f1 = w;
	    if (!p)
	      continue;
	    return 0;
	  }
      for (;;)
	*q = 0;
    }
  return 0;
}

static void fn3 ()
{
  fn2 (c);
  l = c.f1 < b;
  if (l)
    {
    L1:
      e = l | j / e % ~f;
      j = f - 4 % k < c.f1 / e / b - j - 1;
      if (l)
	{
	  __builtin_printf ("%d\n", b);
	  goto L1;
	}
      int m[245];
      g = m[2];
    }
}

int main ()
{
  fn3 ();
  if (c.f1 != 1)
    abort ();
  return 0;
}
