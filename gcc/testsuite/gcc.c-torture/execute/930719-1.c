/* { dg-additional-options "-std=gnu89" } */

int
f (foo, bar, com)
{
  unsigned align;
  if (foo)
    return 0;
  while (1)
    {
      switch (bar)
	{
	case 1:
	  if (com != 0)
 	    return align;
	  *(char *) 0 = 0;
	}
    }
}

main ()
{
  f (0, 1, 1);
  exit (0);
}
