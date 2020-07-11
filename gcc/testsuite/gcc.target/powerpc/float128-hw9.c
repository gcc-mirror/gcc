/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-require-effective-target float128 } */
/* { dg-options "-mpower9-vector -O2 -ffast-math" } */

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
