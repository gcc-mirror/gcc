int a, b;

void
fn1 ()
{
  for (;;)
    {
      for (b = 0; b < 3; b++)
	{
	  char e[2];
	  char f = e[1];
	  a ^= f ? 1 / f : 0;
	}
    }
}
