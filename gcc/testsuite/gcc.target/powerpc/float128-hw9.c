/* { dg-do compile { target lp64 } } */
/* { dg-options "-mvsx -O2 -ffast-math" } */
/* { dg-additional-options "-mdejagnu-cpu=power9" { target { ! has_arch_pwr9 } } } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-require-effective-target float128 } */

extern _Float128 sqrtf128 (_Float128);

/* Check sqrt optimizations that are done for double are also done for
   _Float128.  */

_Float128
sqrt_x_times_sqrt_x (_Float128 x)
{
  return sqrtf128 (x) * sqrtf128 (x);
}

/* { dg-final { scan-assembler-not {\mxssqrtqp\M} } } */
/* { dg-final { scan-assembler-not {\mbl\M}       } } */
