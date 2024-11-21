/* { dg-options "-O2 -mabi=ibmlongdouble -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-require-effective-target float128 } */

/* Verify there is no lxv.*x? and mfvsrd (vector load and move).  */

int
sbm (_Float128 *a)
{
  return __builtin_signbit (*a);
}

/* { dg-final { scan-assembler-times {\ml(d|wz)\M} 1 } } */
/* { dg-final { scan-assembler-not {\mlxv\M} } } */
/* { dg-final { scan-assembler-not {\mlxvd2x\M} } } */
/* { dg-final { scan-assembler-not {\mmfvsrd\M} } } */
