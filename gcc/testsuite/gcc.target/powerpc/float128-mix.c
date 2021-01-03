/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-options "-O2 -mvsx -Wno-psabi -mabi=ibmlongdouble -mlong-double-128" } */

/* Test to make sure that __float128 and long double cannot be combined
   together, when long double uses the IBM extended double format, and
   __float128 uses the IEEE 128-bit format.  */
__float128
add (__float128 a, long double b)
{
  return a+b;	/* { dg-error "IEEE 128-bit and IBM 128-bit floating point" } */
}

long double
sub (long double a, __float128 b)
{
  return a-b;	/* { dg-error "IEEE 128-bit and IBM 128-bit floating point" } */
}
