main ()
{
  int i = 0;
  int a (int x)
    {
      while (x)
	i++, x--;
      return x;
    }
  a (2);
  exit (0);
}
