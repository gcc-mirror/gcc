int array[10] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1};

main ()
{
  int i, j;
  int *p;

  for (i = 0; i < 10; i++)
    for (p = &array[0]; p != &array[9]; p++)
      if (*p == i)
	goto label;

 label:
  if (i != 1)
    abort ();
  exit (0);
}
