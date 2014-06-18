/* { dg-do run } */

int a, b, c[1], d, e, f;

void
fn1 ()
{
  for (; d < 1; d++)
    {
      if (b)
	{
	  a = e++ && f;
	  b = f;
	}
      c[b] = 0;
    }
}

int
main ()
{
  fn1 ();

  if (e != 0)
    __builtin_abort ();

  return 0;
}

