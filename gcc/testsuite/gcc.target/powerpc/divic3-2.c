/* { dg-do compile } */
/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-require-effective-target longdouble128 } */
/* { dg-options "-O2 -mabi=ibmlongdouble -Wno-psabi" } */

/* When GCC is configured with an older library that does not support IEEE
   128-bit, it issues a warning if you change the long double type. We use
   -Wno-psabi to silence this warning.  Since this is a code generation test,
   it does not matter if the library has full IEEE 128-bit support.

   We also need to require that the default long double is 128-bits, otherwise
   the TC/TF modes might not be available.  */

/* Check that complex divide generates the right call for __ibm128 when long
   double is IBM 128-bit floating point.  */

typedef _Complex long double c_ibm128_t __attribute__((mode(__TC__)));

void
divide (c_ibm128_t *p, c_ibm128_t *q, c_ibm128_t *r)
{
  *p = *q / *r;
}

/* { dg-final { scan-assembler {\mbl .*__divtc3\M} } } */
