// PR target/83252
// { dg-do run }
// { dg-options "-O3" }
// { dg-additional-options "-mbmi2 -mtune=intel" { target bmi2 } }
// { dg-additional-options "-mclear-hwcap" { target *-*-solaris* } }

#if __SIZEOF_INT__ == 4 && __SIZEOF_LONG_LONG__ == 8 && __CHAR_BIT__ == 8

#ifdef __BMI2__
#include "../../gcc.target/i386/bmi2-check.h"
#endif

long long h = 707493562598231894LL, i, n, x3, x5;
long long j, l = -2228108721620697360LL, o, y9;
int k, p, r, s, t = 2, u, w, z8, x7, y4, y5, y6, y7, y8, x1, x2, x4, x6, d;
unsigned v, x = 751359462, z = 1, y3 = 60;
unsigned *y = &x, *z2 = &z, *z3 = &v;
unsigned long long z1 = 2;
unsigned long long *z4 = &z1;
long long *z7;
unsigned long long z9 = 7091529791657LL;

void
foo ()
{
  if ((-2783342978U * (int) l || z) && z2 && h && z1 && z9 & ~-(-2783342978U * (int) l))
    {
      i = 3060393125LL < n;
      y7 = o >> *y - 751359400;
      *z3 = x7;
      long a = (o >> *y - 751359400 >> ~-(-2783342978U * (int) l) - 88480234) - (-2783342978U * (int) l);
      y6 = a;
      if (~0 % *z4 % 5)
	y8 = -3 * (l - 4 ? : 407228174574);
      if (y3 < 1)
	{
	  long long *b = &y9;
	  z3 = 0;
	  int c = *z2;
	  *z7 = 0;
	  x1 = ~(-((unsigned) (-2783342978U * (unsigned long long) l)));
	  p = *b & j;
	  x2 = c;
	}
      else
	{
	  j = 0;
	  int e = !0 % (7 % *z4);
	  r = ((s || !k) && t) - -(-2783342978U * (unsigned long long) l);
	  x3 = o >> *y - 751359400;
	  y9 = z9;
	  long f = o >> *y - 751359400 >> ~-(-2783342978U * (int) l) - 88480234;
	  x4 = z1;
	  u = n * f * e * y4;
	}
      if (8ULL * -(-(-2783342978U * (int) l)))
	;
      else
	{
	  *z3 = 0;
	  int g = 3 & y5;
	  x5 = (unsigned) (~o + 9223372036854775807 >> l);
	  z8 = g + y9;
	  v = j || ~0 + 9223372036854775807 >> ~-(-2783342978U * (int) l);
	  x6 = o >> (8 * l);
	  w = *y ? -2783342978U * l : 0;
	}
    }
}

#ifdef __BMI2__
void
bmi2_test (void)
{
  foo ();
  if (r != 88480289)
    __builtin_abort ();
}
#else
int
main ()
{
  foo ();
  if (r != 88480289)
    __builtin_abort ();
}
#endif
#else
int
main ()
{
}
#endif
