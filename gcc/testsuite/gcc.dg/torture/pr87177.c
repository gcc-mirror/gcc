/* { dg-do run } */

int __attribute__((noinline)) my_printf (const char *p, ...)
{
  static volatile int x;
  ++x;
}

int a, b, c, e, f, g, h, i, j, k, l;
unsigned d;

static void p ()
{
  while (1)
    {
      int n = h;
      h = 8;
      if (!e)
	break;
      h = n;
      while (1)
	;
    }
  for (; c != 4; c++)
    {
      int o = g = 1;
      for (; g; g--)
	{
	  while (d < b)
	    e--;
	  a = g;
	  int q = o;
	  if (q)
	    L1:
		j = f;
	  if (l)
	    {
	      my_printf ("%d", g);
	      goto L1;
	    }
	  o = l;
	  k = q;
	}
    }
}

void s ()
{
  int m = 0;
L2:
  if (i && g)
    goto L2;
  for (; m < 2; m++)
    p ();
}

int main ()
{
  s ();
  return 0;
}
