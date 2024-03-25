int
main (void)
{
  int i;
  int f;

  for (i = 0;; i--)
    {
      f = 0;

      if ((i & (i - 1)) == 0)
	{
	  __builtin_printf ("d");
	  f = 1;
	}
      if ((i & -i) == i)
	{
	  __builtin_printf ("t");
	  f = 1;
	}
      if (f)
	__builtin_printf ("%d\n", i);
    }
}
