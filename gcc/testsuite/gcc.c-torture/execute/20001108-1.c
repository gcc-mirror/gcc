long long
poly (long long sum, long x)
{
  sum += (long long) (long) sum * (long long) x;
  return sum;
}

int
main (void)
{
  if (poly (2LL, 3) != 8LL)
    abort ();

  exit (0);
}
