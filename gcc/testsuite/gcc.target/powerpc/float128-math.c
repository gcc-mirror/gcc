/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-mvsx -O2 -mfloat128 -mabi=ieeelongdouble -Wno-psabi" } */

/* Test whether we convert __builtin_<math>l to __builtin_<math>f128 if the
   default long double type is IEEE 128-bit.  Also test that using the explicit
   __builtin_<math>f128 function does not interfere with the __builtin_<math>l
   function.  */

extern __float128 sinf128 (__float128);

void foo (__float128 *p, long double *q, long double *r)
{
  *p = sinf128 (*p);
  *q = __builtin_sinl (*q);
}

/* { dg-final { scan-assembler-times {\mbl sinf128\M} 2 } } */
/* { dg-final { scan-assembler-not   {\mbl sinl\M}      } } */
