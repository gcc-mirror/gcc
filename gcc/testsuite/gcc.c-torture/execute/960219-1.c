f (int i)
{
  if (((1 << i) & 1) == 0)
    abort ();
}

main ()
{
  f (0);
  exit (0);
}
