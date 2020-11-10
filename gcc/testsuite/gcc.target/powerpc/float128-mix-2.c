/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-options "-O2 -mvsx -Wno-psabi -mabi=ieeelongdouble -mlong-double-128" } */

/* Test to make sure that __float128 and long double do not generate errors if
   long double uses the IEEE 128-bit format.  */
__float128
add (__float128 a, long double b)
{
  return a+b;
}

long double
sub (long double a, __float128 b)
{
  return a-b;
}
