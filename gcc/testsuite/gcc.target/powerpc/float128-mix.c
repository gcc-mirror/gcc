/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target powerpc_float128_sw_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O2 -mcpu=power7 -mfloat128" } */


/* Test to make sure that __float128 and __ibm128 cannot be combined together.  */
__float128 add (__float128 a, __ibm128 b)
{
  return a+b;	/* { dg-error "__float128 and __ibm128 cannot be used in the same expression" "" } */
}

__ibm128 sub (__ibm128 a, __float128 b)
{
  return a-b;	/* { dg-error "__float128 and __ibm128 cannot be used in the same expression" "" } */
}
