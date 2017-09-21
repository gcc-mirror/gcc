/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mpower9-vector -mno-float128-hardware -O2" } */

__float128
xfma (__float128 a, __float128 b, __float128 c)
{
  return __builtin_fmaf128 (a, b, c); /* { dg-error "ISA 3.0 IEEE 128-bit" } */
}
