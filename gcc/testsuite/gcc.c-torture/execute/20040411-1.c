void abort (void);

int
sub1 (int i, int j)
{
  typedef int c[i+2];
  int x[10], y[10];

  if (j == 2)
    {
      __builtin_memcpy (x, y, 10 * sizeof (int));
      return sizeof (c);
    }
  else
    return sizeof (c) * 3;
}

int
main ()
{
  if (sub1 (20, 3) != 66 * sizeof (int))
    abort ();

  return 0;
}
