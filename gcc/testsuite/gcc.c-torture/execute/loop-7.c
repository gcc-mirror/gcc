void foo (unsigned int n)
{
  int i, j = -1;

  for (i = 0; i < 10 && j < 0; i++)
    {
      if ((1UL << i) == n)
	j = i;
    }

  if (j < 0)
    abort ();
}

main()
{
  foo (64);
  exit (0);
}
