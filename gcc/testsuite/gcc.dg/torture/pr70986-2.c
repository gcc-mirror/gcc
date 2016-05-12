/* { dg-do compile } */

int gi, dg;

void
fe (void)
{
  int ka = gi;

  for (;;)
    {
      if (ka != 0)
	{
	  if (dg != 0)
	    gi = 0;
	  ++ka;
	}
      ++dg;
    }
}
