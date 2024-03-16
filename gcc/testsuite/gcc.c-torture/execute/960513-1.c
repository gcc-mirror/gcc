void abort (void);
void exit (int);

long double
f (d, i)
     long double d;
     int i;
{
  long double e;

  d = -d;
  e = d;
  if (i == 1)
    d *= 2;
  d -= e * d;
  d -= e * d;
  d -= e * d;
  d -= e * d;
  d -= e * d;
  return d;
}

int
main (void)
{
  if (! (int) (f (2.0L, 1)))
    abort ();
  exit (0);
}
