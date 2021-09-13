/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-mvsx -O2 -mfloat128 -mlong-double-128 -mabi=ieeelongdouble -Wno-psabi" } */

/* Test whether we convert __builtin_<math>l to __builtin_<math>f128 if the
   default long double type is IEEE 128-bit.  We leave off the \M in matching
   the calls, so power10 will match using bl foo@notoc.  Also test that using
   the explicit __builtin_<math>f128 function does not interfere with the
   __builtin_<math>l function.  */

extern __float128 sinf128 (__float128);

void foo (__float128 *p, long double *q)
{
  *p = sinf128 (*p);
  *q = __builtin_sinl (*q);
}

/* { dg-final { scan-assembler     {\mbl __sinieee128} } } */
/* { dg-final { scan-assembler-not {\mbl sinl}         } } */
