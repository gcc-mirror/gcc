/* { dg-do compile } */

int bh, on, h0;

void
qw (int n2)
{
  int *e5;

  if (n2 == 0)
    {
      n2 = 1;
      while (n2 != 0)
	for (n2 = 0; n2 < 1; ++n2)
	  {
	  }

      e5 = &n2;
    }
  else
    e5 = &on;

  while (h0 < 1)
    {
      if (on == 0)
	{
	  ++*e5;
	  bh = 0;
	}
      else
	{
	  bh = 0;
	  ++on;
	  *e5 = on;
	  h0 = *e5;
	  if (h0 == 0)
	    {
	      *e5 = 0;
	      ++h0;
	    }
	}

      ++h0;
    }
}
