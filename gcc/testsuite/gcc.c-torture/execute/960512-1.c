__complex__
double f ()
{
  int a[40];
  __complex__ double c;

  a[9] = 0;
  c = a[9];
  return c;
}

main ()
{
  __complex__ double c;

  if (c = f ())
    abort ();
  exit (0);
}
