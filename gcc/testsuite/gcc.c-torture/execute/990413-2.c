/* This tests for a bug in regstack that was breaking glibc's math library. */

extern void abort (void);

static __inline double
minus_zero (void)
{
  union { double __d; int __i[2]; } __x;
  __x.__i[0] = 0x0;
  __x.__i[1] = 0x80000000;
  return __x.__d;
}

static __inline long double
__atan2l (long double __y, long double __x)
{
  register long double __value;
  __asm __volatile__ ("fpatan\n\t"
		      : "=t" (__value)
		      : "0" (__x), "u" (__y)
		      : "st(1)");
  return __value;
}

static __inline long double
__sqrtl (long double __x)
{
  register long double __result;
  __asm __volatile__ ("fsqrt" : "=t" (__result) : "0" (__x));
  return __result;
}

static __inline double
asin (double __x)
{
  return __atan2l (__x, __sqrtl (1.0 - __x * __x));
}

int
main (void)
{
  double x;

  x = minus_zero();
  x = asin (x);

  if (x != 0.0) /* actually -0.0, but 0.0 == -0.0 */
    abort ();
  return 0;
}
