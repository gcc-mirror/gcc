/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-options "-mvsx -mfloat128 -O2 -mabi=ibmlongdouble -Wno-psabi" } */

// PR 85657 -- make sure conversions work between each of the 128-bit floating
// point types.

__attribute__ ((__noinline__))
__float128
ibm128_to_float128 (__ibm128 a)
{
  return (__float128)a;
}

__attribute__ ((__noinline__))
__float128
ldouble_to_float128 (long double a)
{
  return (__float128)a;
}

__attribute__ ((__noinline__))
__ibm128
float128_to_ibm128 (__float128 a)
{
  return (__ibm128)a;
}

__attribute__ ((__noinline__))
__ibm128
ldouble_to_ibm128 (long double a)
{
  return (__ibm128)a;
}

__attribute__ ((__noinline__))
long double
ibm128_to_ldouble (__ibm128 a)
{
  return (long double)a;
}

__attribute__ ((__noinline__))
long double
float128_to_ldouble (__float128 a)
{
  return (long double)a;
}

#ifdef TEST
#include <stdio.h>

volatile __float128 f128 = 1.2Q;
volatile __ibm128 i128 = (__ibm128)3.4L;
volatile long double ld = 5.6L;

int
main (void)
{
  printf ("f128 (1.2) = %g (ld), %g (ibm128)\n",
	  (double) float128_to_ldouble (f128),
	  (double) float128_to_ibm128 (f128));

  printf ("i128 (3.4) = %g (ld), %g (float128)\n",
	  (double) ibm128_to_ldouble (i128),
	  (double) ibm128_to_float128 (i128));

  printf ("long double (5.6) = %g (ibm128), %g (float128)\n",
	  (double) ldouble_to_ibm128 (ld),
	  (double) ldouble_to_float128 (ld));

  return 0;
}
#endif
