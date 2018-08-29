/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-mvsx -O2" } */

/* PR 85657 -- make __ibm128 a full type.  */

__attribute__ ((__noinline__))
__float128
ibm128_to_float128 (__ibm128 a)
{
  return (__float128)a + 1.0q;
}

__attribute__ ((__noinline__))
__float128
ldouble_to_float128 (long double a)
{
  return (__float128)a + 1.0q;
}

__attribute__ ((__noinline__))
__ibm128
float128_to_ibm128 (__float128 a)
{
  return (__ibm128)a + (__ibm128)1.0;
}

__attribute__ ((__noinline__))
__ibm128
ldouble_to_ibm128 (long double a)
{
  return (__ibm128)a + (__ibm128)1.0;
}

__attribute__ ((__noinline__))
long double
ibm128_to_ldouble (__ibm128 a)
{
  return (long double)a + 1.0L;
}

__attribute__ ((__noinline__))
long double
float128_to_ldouble (__float128 a)
{
  return (long double)a + 1.0L;
}

volatile __float128  f128 = 1.25Q;
volatile __ibm128    i128 = (__ibm128)3.5L;
volatile long double ld   = 4.75L;

volatile double f128_p1 = 2.25;
volatile double i128_p1 = 4.5;
volatile double ld_p1   = 5.75;

extern void abort (void);

int
main (void)
{
  if (((double) float128_to_ldouble (f128)) != f128_p1)
    abort ();

  if (((double) float128_to_ibm128 (f128)) != f128_p1)
    abort ();

  if (((double) ibm128_to_ldouble (i128)) != i128_p1)
    abort ();

  if (((double) ibm128_to_float128 (i128)) != i128_p1)
    abort ();

  if (((double) ldouble_to_ibm128 (ld)) != ld_p1)
    abort ();

  if (((double) ldouble_to_float128 (ld)) != ld_p1)
    abort ();

  return 0;
}
