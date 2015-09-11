/* { dg-do compile } */

int a, b;

int
fn1 (int p)
{
  return 0 == 0 ? p : 0;
}

void
fn2 ()
{
  int c = 1, d[1] = { 1 };
lbl:
  for (;;)
    {
      int e;
      c ? 0 : 0 / c;
      c = 0;
      if (fn1 (d[0]))
	break;
      for (e = 0; e < 1; e++)
	for (c = 1; b;)
	  {
	    if (a)
	      break;
	    goto lbl;
	  }
    }
}
