/* { dg-do run } */

int a, b = 1, c, d, e, f, g;

int
fn1 ()
{
  int h;
  for (;;)
    {
      g = b;
      g = g ? 0 : 1 % g;
      e = a + 1;
      for (; d < 1; d = e)
	{
	  if (f == 0)
	    h = 0;
	  else
	    h = 1 % f;
	  if (f < 1)
	    c = 0;
	  else if (h)
	    break;
	}
      if (b)
	return 0;
    }
}

int
main ()
{
  fn1 ();
  return 0;
}
