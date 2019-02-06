/* { dg-do compile  { target { powerpc*-*-* && lp64 } } } */
/* { dg-options "-O2 -fstack-protector-strong -mlong-double-128" } */

extern long double foo (long double);
extern double bar (double);
typedef long long int64_t;
typedef unsigned long long uint64_t;
typedef union { int64_t i[2]; long double x; double d[2]; } mynumber;
static const double t512 = 0x1p512, tm256 = 0x1p-256, two54 = 0x1p54, twom54 = 0x1p-54;

long double
foo (long double x)
{
  static const long double big = 134217728.0, big1 = 134217729.0;
  long double t, s, i;
  mynumber a, c;
  uint64_t k, l;
  int64_t m, n;
  double d;

  a.x = x;
  k = a.i[0] & 0x7fffffffffffffffL;

  if (k > 0x000fffff00000000L && k < 0x7ff0000000000000L)
    {
      if (x < 0)
	return (big1 - big1) / (big - big);
      l = (k & 0x001fffffffffffffL) | 0x3fe0000000000000L;
      if ((a.i[1] & 0x7fffffffffffffffL) != 0)
	{
	  n = (int64_t) ((l - k) * 2) >> 53;
	  m = (a.i[1] >> 52) & 0x7ff;
	  if (m == 0)
	    {
	      a.d[1] *= two54;
	      m = ((a.i[1] >> 52) & 0x7ff) - 54;
	    }
	  m += n;
	  if (m > 0)
	    a.i[1] = (a.i[1] & 0x800fffffffffffffL) | (m << 52);
	  else if (m <= -54)
	    {
	      a.i[1] &= 0x8000000000000000L;
	    }
	  else
	    {
	      m += 54;
	      a.i[1] = (a.i[1] & 0x800fffffffffffffL) | (m << 52);
	      a.d[1] *= twom54;
	    }
	}
      a.i[0] = l;
      s = a.x;
      d = bar (a.d[0]);
      c.i[0] = 0x2000000000000000L + ((k & 0x7fe0000000000000L) >> 1);
      c.i[1] = 0;
      i = d;
      t = 0.5L * (i + s / i);
      i = 0.5L * (t + s / t);
      return c.x * i;
    }
  else
    {
      if (k >= 0x7ff0000000000000L)

	return x * x + x;
      if (x == 0)
	return x;
      if (x < 0)
	return (big1 - big1) / (big - big);
      return tm256 * foo (x * t512);
    }
}
