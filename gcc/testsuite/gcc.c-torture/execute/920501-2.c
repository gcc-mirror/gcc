void abort (void);
void exit (int);

unsigned long
gcd_ll (unsigned long long x, unsigned long long y)
{
  for (;;)
    {
      if (y == 0)
	return (unsigned long) x;
      x = x % y;
      if (x == 0)
	return (unsigned long) y;
      y = y % x;
    }
}

unsigned long long
powmod_ll (unsigned long long b, unsigned e, unsigned long long m)
{
  unsigned t;
  unsigned long long pow;
  int i;

  if (e == 0)
    return 1;

  /* Find the most significant bit in E.  */
  t = e;
  for (i = 0; t != 0; i++)
    t >>= 1;

  /* The most sign bit in E is handled outside of the loop, by beginning
     with B in POW, and decrementing I.  */
  pow = b;
  i -= 2;

  for (; i >= 0; i--)
    {
      pow = pow * pow % m;
      if ((1 << i) & e)
	pow = pow * b % m;
    }

  return pow;
}

unsigned long factab[10];

void
facts (t, a_int, x0, p)
     unsigned long long t;
     int a_int;
     int x0;
     unsigned p;
{
  unsigned long *xp = factab;
  unsigned long long x, y;
  unsigned long q = 1;
  unsigned long long a = a_int;
  int i;
  unsigned long d;
  int j = 1;
  unsigned long tmp;
  int jj = 0;

  x = x0;
  y = x0;

  for (i = 1; i < 10000; i++)
    {
      x = powmod_ll (x, p, t) + a;
      y = powmod_ll (y, p, t) + a;
      y = powmod_ll (y, p, t) + a;

      if (x > y)
	tmp = x - y;
      else
	tmp = y - x;
      q = (unsigned long long) q * tmp % t;

      if (i == j)
	{
	  jj += 1;
	  j += jj;
	  d = gcd_ll (q, t);
	  if (d != 1)
	    {
	      *xp++ = d;
	      t /= d;
	      if (t == 1)
		{
		  return;
		  *xp = 0;
		}
	    }
	}
    }
}

int
main (void)
{
  unsigned long long t;
  unsigned x0, a;
  unsigned p;

  p = 27;
  t = (1ULL << p) - 1;

  a = -1;
  x0 = 3;

  facts (t, a, x0, p);
  if (factab[0] != 7 || factab[1] != 73 || factab[2] != 262657)
    abort();
  exit (0);
}
