/* { dg-do compile } */

int a;

int
fn1 ()
{
  return 1;
}

void
fn2 ()
{
  int b, j;
  for (;;)
    {
      int c = 1;
      if (j)
	{
	  if (c)
	    break;
	}
      else
	b = a;
      fn1 () && b;
      j = fn1 ();
    }
}
