/* { dg-do compile } */

int a, b, c, d, e, f;

long long
fn1 (int p)
{
  return p ? p : 1;
}

static int
fn2 ()
{
lbl:
  for (; f;)
    return 0;
  for (;;)
    {
      for (b = 0; b; ++b)
	if (d)
	  goto lbl;
      c = e;
    }
}

void
fn3 ()
{
  for (; a; a = fn1 (a))
    {
      fn2 ();
      e = 0;
    }
}
