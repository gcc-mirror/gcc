/* Test gcse handling of IEEE 0/-0 rules.  */
static double zero = 0.0;

int
negzero_check (double d)
{
  if (d == 0)
    return !!memcmp ((void *)&zero, (void *)&d, sizeof (double));
  return 0;
}

int
sub (double d, double e)
{
  if (d == 0.0 && e == 0.0
      && negzero_check (d) == 0 && negzero_check (e) == 0)
    return 1;
  else
    return 0;
}

int
main (void)
{
  double minus_zero = -0.0;
  if (sub (minus_zero, 0))
    abort ();
  return 0;
}
