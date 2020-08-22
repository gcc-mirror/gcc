/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-require-effective-target float128 } */
/* { dg-options "-mpower9-vector -O2" } */

__float128
xsqrt (__float128 a)
{
  return __builtin_sqrtf128 (a);
}

/* { dg-final { scan-assembler "xssqrtqp"  } } */
