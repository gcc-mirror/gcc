/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O3 -ftree-vectorize -mcpu=power7 -ffast-math -mveclibabi=mass" } */
/* { dg-final { scan-assembler "bl atan2d2" } } */
/* { dg-final { scan-assembler "bl atan2f4" } } */
/* { dg-final { scan-assembler "bl hypotd2" } } */
/* { dg-final { scan-assembler "bl hypotf4" } } */
/* { dg-final { scan-assembler "bl powd2" } } */
/* { dg-final { scan-assembler "bl powf4" } } */
/* { dg-final { scan-assembler "bl acosd2" } } */
/* { dg-final { scan-assembler "bl acosf4" } } */
/* { dg-final { scan-assembler "bl acoshd2" } } */
/* { dg-final { scan-assembler "bl acoshf4" } } */
/* { dg-final { scan-assembler "bl asind2" } } */
/* { dg-final { scan-assembler "bl asinf4" } } */
/* { dg-final { scan-assembler "bl asinhd2" } } */
/* { dg-final { scan-assembler "bl asinhf4" } } */
/* { dg-final { scan-assembler "bl atand2" } } */
/* { dg-final { scan-assembler "bl atanf4" } } */
/* { dg-final { scan-assembler "bl atanhd2" } } */
/* { dg-final { scan-assembler "bl atanhf4" } } */
/* { dg-final { scan-assembler "bl cbrtd2" } } */
/* { dg-final { scan-assembler "bl cbrtf4" } } */
/* { dg-final { scan-assembler "bl cosd2" } } */
/* { dg-final { scan-assembler "bl cosf4" } } */
/* { dg-final { scan-assembler "bl coshd2" } } */
/* { dg-final { scan-assembler "bl coshf4" } } */
/* { dg-final { scan-assembler "bl erfd2" } } */
/* { dg-final { scan-assembler "bl erff4" } } */
/* { dg-final { scan-assembler "bl erfcd2" } } */
/* { dg-final { scan-assembler "bl erfcf4" } } */
/* { dg-final { scan-assembler "bl exp2d2" } } */
/* { dg-final { scan-assembler "bl exp2f4" } } */
/* { dg-final { scan-assembler "bl expd2" } } */
/* { dg-final { scan-assembler "bl expf4" } } */
/* { dg-final { scan-assembler "bl expm1d2" } } */
/* { dg-final { scan-assembler "bl expm1f4" } } */
/* { dg-final { scan-assembler "bl lgamma" } } */
/* { dg-final { scan-assembler "bl lgammaf" } } */
/* { dg-final { scan-assembler "bl log10d2" } } */
/* { dg-final { scan-assembler "bl log10f4" } } */
/* { dg-final { scan-assembler "bl log1pd2" } } */
/* { dg-final { scan-assembler "bl log1pf4" } } */
/* { dg-final { scan-assembler "bl log2d2" } } */
/* { dg-final { scan-assembler "bl log2f4" } } */
/* { dg-final { scan-assembler "bl logd2" } } */
/* { dg-final { scan-assembler "bl logf4" } } */
/* { dg-final { scan-assembler "bl sind2" } } */
/* { dg-final { scan-assembler "bl sinf4" } } */
/* { dg-final { scan-assembler "bl sinhd2" } } */
/* { dg-final { scan-assembler "bl sinhf4" } } */
/* { dg-final { scan-assembler "bl tand2" } } */
/* { dg-final { scan-assembler "bl tanf4" } } */
/* { dg-final { scan-assembler "bl tanhd2" } } */
/* { dg-final { scan-assembler "bl tanhf4" } } */

#ifndef SIZE
#define SIZE 1024
#endif

double d1[SIZE] __attribute__((__aligned__(32)));
double d2[SIZE] __attribute__((__aligned__(32)));
double d3[SIZE] __attribute__((__aligned__(32)));

float f1[SIZE] __attribute__((__aligned__(32)));
float f2[SIZE] __attribute__((__aligned__(32)));
float f3[SIZE] __attribute__((__aligned__(32)));

void
test_double_atan2 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_atan2 (d2[i], d3[i]);
}

void
test_float_atan2 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_atan2f (f2[i], f3[i]);
}

void
test_double_hypot (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_hypot (d2[i], d3[i]);
}

void
test_float_hypot (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_hypotf (f2[i], f3[i]);
}

void
test_double_pow (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_pow (d2[i], d3[i]);
}

void
test_float_pow (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_powf (f2[i], f3[i]);
}

void
test_double_acos (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_acos (d2[i]);
}

void
test_float_acos (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_acosf (f2[i]);
}

void
test_double_acosh (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_acosh (d2[i]);
}

void
test_float_acosh (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_acoshf (f2[i]);
}

void
test_double_asin (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_asin (d2[i]);
}

void
test_float_asin (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_asinf (f2[i]);
}

void
test_double_asinh (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_asinh (d2[i]);
}

void
test_float_asinh (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_asinhf (f2[i]);
}

void
test_double_atan (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_atan (d2[i]);
}

void
test_float_atan (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_atanf (f2[i]);
}

void
test_double_atanh (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_atanh (d2[i]);
}

void
test_float_atanh (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_atanhf (f2[i]);
}

void
test_double_cbrt (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_cbrt (d2[i]);
}

void
test_float_cbrt (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_cbrtf (f2[i]);
}

void
test_double_cos (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_cos (d2[i]);
}

void
test_float_cos (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_cosf (f2[i]);
}

void
test_double_cosh (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_cosh (d2[i]);
}

void
test_float_cosh (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_coshf (f2[i]);
}

void
test_double_erf (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_erf (d2[i]);
}

void
test_float_erf (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_erff (f2[i]);
}

void
test_double_erfc (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_erfc (d2[i]);
}

void
test_float_erfc (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_erfcf (f2[i]);
}

void
test_double_exp2 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_exp2 (d2[i]);
}

void
test_float_exp2 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_exp2f (f2[i]);
}

void
test_double_exp (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_exp (d2[i]);
}

void
test_float_exp (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_expf (f2[i]);
}

void
test_double_expm1 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_expm1 (d2[i]);
}

void
test_float_expm1 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_expm1f (f2[i]);
}

void
test_double_lgamma (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_lgamma (d2[i]);
}

void
test_float_lgamma (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_lgammaf (f2[i]);
}

void
test_double_log10 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_log10 (d2[i]);
}

void
test_float_log10 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_log10f (f2[i]);
}

void
test_double_log1p (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_log1p (d2[i]);
}

void
test_float_log1p (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_log1pf (f2[i]);
}

void
test_double_log2 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_log2 (d2[i]);
}

void
test_float_log2 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_log2f (f2[i]);
}

void
test_double_log (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_log (d2[i]);
}

void
test_float_log (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_logf (f2[i]);
}

void
test_double_sin (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_sin (d2[i]);
}

void
test_float_sin (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_sinf (f2[i]);
}

void
test_double_sinh (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_sinh (d2[i]);
}

void
test_float_sinh (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_sinhf (f2[i]);
}

void
test_double_sqrt (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_sqrt (d2[i]);
}

void
test_float_sqrt (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_sqrtf (f2[i]);
}

void
test_double_tan (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_tan (d2[i]);
}

void
test_float_tan (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_tanf (f2[i]);
}

void
test_double_tanh (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    d1[i] = __builtin_tanh (d2[i]);
}

void
test_float_tanh (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    f1[i] = __builtin_tanhf (f2[i]);
}
