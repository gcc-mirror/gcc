void
fpEq (double x, double y)
{
  if (x != y)
    abort ();
}

void
fpTest (double x, double y)
{
  double result1 = (35.7 * 100.0) / 45.0;
  double result2 = (x * 100.0) / y;
  fpEq (result1, result2);
}

main ()
{
  fpTest (35.7, 45.0);
  exit (0);
}
