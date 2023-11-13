void abort (void);
void exit (int);

int
f (unsigned bitcount, int mant)
{
  int mask = -1 << bitcount;
  {
    if (! (mant & -mask))
      goto ab;
    if (mant & ~mask)
      goto auf;
  }
ab:
  return 0;
auf:
  return 1;
}

int
main (void)
{
  if (f (0, -1))
    abort ();
  exit (0);
}
