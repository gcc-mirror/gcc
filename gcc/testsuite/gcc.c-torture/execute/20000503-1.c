unsigned long
sub (int a)
{
  return ((0 > a - 2) ? 0 : a - 2) * sizeof (long);
}

main ()
{
  if (sub (0) != 0)
    abort ();

  exit (0);
}
