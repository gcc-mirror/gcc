/* { dg-additional-options "-fwrapv" } */
void abort (void);
void exit (int);

int errflag;

long long
f (long long x, long long y)
{
  long long r;

  errflag = 0;
  r = x + y;
  if (x >= 0)
    {
      if ((y < 0) || (r >= 0))
	return r;
    }
  else
    {
      if ((y > 0) || (r < 0))
	return r;
    }
  errflag = 1;
  return 0;
}

int
main (void)
{
  f (0, 0);
  if (errflag)
    abort ();

  f (1, -1);
  if (errflag)
    abort ();

  f (-1, 1);
  if (errflag)
    abort ();

  f (0x8000000000000000LL, 0x8000000000000000LL);
  if (!errflag)
    abort ();

  f (0x8000000000000000LL, -1LL);
  if (!errflag)
    abort ();

  f (0x7fffffffffffffffLL, 0x7fffffffffffffffLL);
  if (!errflag)
    abort ();

  f (0x7fffffffffffffffLL, 1LL);
  if (!errflag)
    abort ();

  f (0x7fffffffffffffffLL, 0x8000000000000000LL);
  if (errflag)
    abort ();

  exit (0);
}
