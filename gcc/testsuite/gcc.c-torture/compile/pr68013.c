int a, b, c, d, e, f;

void
fn1 ()
{
  for (; e;)
    {
      e = f;
      for (; b;)
	{
	  b = a;
	  f = a || d ? 0 : c;
	}
      d = 0;
    }
}
