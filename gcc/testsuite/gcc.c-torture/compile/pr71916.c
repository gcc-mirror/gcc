/* PR rtl-optimization/71916 */

int a, b, c, d, f, g;
short h;

short
foo (short p1)
{
  return a >= 2 || p1 > 7 >> a ? p1 : p1 << a;
}

void
bar (void)
{
  for (;;)
    {
      int i, j[3];
      h = b >= 2 ? d : d >> b;
      if (foo (f > h ^ c))
	{
	  d = 0;
	  while (f <= 2)
	    {
	      char k[2];
	      for (;;)
		k[i++] = 7;
	    }
	}
      else
	for (;;)
	  g = j[2];
      if (g)
	for (;;)
	  ;
    }
}
