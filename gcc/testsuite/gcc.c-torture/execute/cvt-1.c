static inline long
g1 (double x)
{
  return (double) (long) x;
}

long
g2 (double f)
{
  return f;
}

double
f (long i)
{
  if (g1 (i) != g2 (i))
    abort ();
  return g2 (i);
}

main ()
{
  if (f (123456789L) != 123456789L)
    abort ();
  if (f (123456789L) != g2 (123456789L))
    abort ();
  exit (0);
}
