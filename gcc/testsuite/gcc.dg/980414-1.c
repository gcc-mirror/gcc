/* Test double on x86. */

/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options -O2 } */

extern void abort (void);

static __inline  double
mypow (double __x, double __y)
{
  register double __value, __exponent;
  long __p = (long) __y;
  if (__y == (double) __p)
    {
      double __r = 1.0;
      if (__p == 0)
	return 1.0;
      if (__p < 0)
	{
	  __p = -__p;
	  __x = 1.0 / __x;
	}
      while (1)
	{
	  if (__p & 1)
	    __r *= __x;
	  __p >>= 1;
	  if (__p == 0)
	    return __r;
	  __x *= __x;
	}
    }
  __asm __volatile__
    ("fmul	%%st(1),%%st\n\t"	/* y * log2(x) */
     "fst	%%st(1)\n\t"
     "frndint\n\t"			/* int(y * log2(x)) */
     "fxch  %%st(1)\n\t"
     "fsub	%%st(1),%%st\n\t"	/* fract(y * log2(x)) */
     "f2xm1\n\t"			/* 2^(fract(y * log2(x))) - 1 */
     : "=t" (__value), "=u" (__exponent) :  "0" (__x), "1" (__y));
  __value += 1.0;
  __asm __volatile__
    ("fscale"
     : "=t" (__value) : "0" (__value), "u" (__exponent));
  return __value;
}

const double E1 = 2.71828182845904523536028747135;

double fact (double x)
{
  double corr;
  corr = 1.0;
  return corr * mypow(x/E1, x);
}

int main ()
{
  double y, z;

  y = fact (46.2);
  z = mypow (46.2/E1, 46.2);

#if 0
  printf ("%26.19e, %26.19e\n", y, z);
#endif

  if (y > z)
    y -= z;
  else
    y = z - y;

  y /= z;
  if (y > 0.1)
    abort ();
 
  return 0;
}
