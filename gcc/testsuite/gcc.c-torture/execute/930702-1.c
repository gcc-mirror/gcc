fp (double a, int b)
{
  if (a != 33 || b != 11)
    abort ();
}

main ()
{
  int (*f) (double, int) = fp;

  fp (33, 11);
  f (33, 11);
  exit (0);
}
