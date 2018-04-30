/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */


/* Test to make sure that __float128 and long double cannot be combined together.  */
__float128 add (__float128 a, long double b)
{
  return a+b;	/* { dg-error "__float128 and long double cannot be used in the same expression" } */
}

__ibm128 sub (long double a, __float128 b)
{
  return a-b;	/* { dg-error "__float128 and long double cannot be used in the same expression" } */
}
