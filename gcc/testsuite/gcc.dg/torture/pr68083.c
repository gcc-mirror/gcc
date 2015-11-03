/* { dg-do run } */

int a = 2, b = 1, c = 1;

int
fn1 ()
{
  int d;
  for (; a; a--)
    {
      for (d = 0; d < 4; d++)
	{
	  int k;
	  if (c < 1)
	    if (k)
	      c = 0;
	  if (b)
	    continue;
	  return 0;
	}
      b = !1;
    }
  return 0;
}

int
main ()
{
  fn1 ();

  if (a != 1)
    __builtin_abort ();

  return 0;
}
