main()
{
  double db1 = 1.7976931348623157e+308;
  long double ldb1 = db1;

  if (sizeof (double) != 8 || sizeof (long double) != 16)
    exit (0);

  if (ldb1 != 1.7976931348623157e+308)
    abort ();
  exit (0);
}
