/* Test that the compiler properly generates floating point multiply
   and add instructions FMA3 systems.  */

/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mfma -mno-fma4" } */

#ifndef __FP_FAST_FMAF
# error "__FP_FAST_FMAF should be defined"
#endif
#ifndef __FP_FAST_FMA
# error "__FP_FAST_FMA should be defined"
#endif

float
flt_mul_add (float a, float b, float c)
{
  return __builtin_fmaf (a, b, c);
}

double
dbl_mul_add (double a, double b, double c)
{
  return __builtin_fma (a, b, c);
}

float
flt_mul_sub (float a, float b, float c)
{
  return __builtin_fmaf (a, b, -c);
}

double
dbl_mul_sub (double a, double b, double c)
{
  return __builtin_fma (a, b, -c);
}

float
flt_neg_mul_add_1 (float a, float b, float c)
{
  return __builtin_fmaf (-a, b, c);
}

double
dbl_neg_mul_add_1 (double a, double b, double c)
{
  return __builtin_fma (-a, b, c);
}

float
flt_neg_mul_add_2 (float a, float b, float c)
{
  return __builtin_fmaf (a, -b, c);
}

double
dbl_neg_mul_add_2 (double a, double b, double c)
{
  return __builtin_fma (a, -b, c);
}

float
flt_neg_mul_sub (float a, float b, float c)
{
  return __builtin_fmaf (-a, b, -c);
}

double
dbl_neg_mul_sub (double a, double b, double c)
{
  return __builtin_fma (-a, b, -c);
}

/* { dg-final { scan-assembler-times "vfmadd...ss" 1 } } */
/* { dg-final { scan-assembler-times "vfmadd...sd" 1 } } */
/* { dg-final { scan-assembler-times "vfmsub...ss" 1 } } */
/* { dg-final { scan-assembler-times "vfmsub...sd" 1 } } */
/* { dg-final { scan-assembler-times "vfnmadd...ss" 2 } } */
/* { dg-final { scan-assembler-times "vfnmadd...sd" 2 } } */
/* { dg-final { scan-assembler-times "vfnmsub...ss" 1 } } */
/* { dg-final { scan-assembler-times "vfnmsub...sd" 1 } } */
