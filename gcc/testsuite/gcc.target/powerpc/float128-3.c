/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-options "-O2 -mvsx -mno-float128" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Test that we can use #pragma GCC target to enable -mfloat128.  */

#ifdef __FLOAT128__
#error "-mno-float128 should disable initially defining __FLOAT128__"
#endif

#pragma GCC target("float128")

#ifndef __FLOAT128__
#error "#pragma GCC target(\"float128\") should enable -mfloat128"
#endif

__float128
qadd (__float128 a, __float128 b)
{
  return a+b;
}
