/* { dg-do run } */

int a, b, *c, d, e;

void abort (void);

int
main ()
{
  int f, *g, **h = &g;
  for (; b;)
    {
      c = &a;
      for (e = 0; e < 1; e++)
	*h = 0;
      for (; d; d++)
	if (f)
	  *c = 0;
	else
	  {
	    *c = e = 0;
	    *h = &a;
	  }

      if (a && !g)
	abort ();

    }
  return 0;
}
