/* { dg-do run } */
/* { dg-options "-O3 -fsanitize=unreachable" } */


int a, b, c, d;

void
fn1 ()
{
  for (c = 0; c < 2; c++)
    {
      int e, f = 1;
      for (e = 0; e < 2; e++)
	{
	  if (!f)
	    return;
	  for (d = 0; d; d++)
	    f = b;
	}
    }
}

int
main ()
{
  for (; a < 1; a++)
    {
      fn1 ();
    }
  __builtin_exit (0);
}
