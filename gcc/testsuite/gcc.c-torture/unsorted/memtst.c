memtst (int *p, int a)
{
  do
    {
      if (p[a] == 1)
	break;
    }
  while (--a);
}

main ()
{
  int a[65536];
  int i;
  bzero (a, 65536 * 4);
  for (i = 0;  i < 100;  i++)
    {
      memtst (a, 65536);
    }
}
