/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-options "-O2 -mvsx -Wno-psabi -mabi=ibmlongdouble -mlong-double-128" } */

/* Test to make sure that __float128 and __ibm128 cannot be combined
   together.  */
__float128
add (__float128 a, __ibm128 b)
{
  return a+b;	/* { dg-error "IEEE 128-bit and IBM 128-bit floating point" } */
}

__ibm128
sub (__ibm128 a, __float128 b)
{
  return a-b;	/* { dg-error "IEEE 128-bit and IBM 128-bit floating point" } */
}
