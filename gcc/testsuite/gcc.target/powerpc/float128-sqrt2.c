/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mpower9-vector -mno-float128-hardware -O2" } */

__float128
xsqrt (__float128 a)
{
  return __builtin_sqrtf128 (a); /* { dg-error "ISA 3.0 IEEE 128-bit" } */
}
