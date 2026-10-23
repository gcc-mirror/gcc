/* { dg-do compile { target { { ! ia32 } && glibc } } } */
/* { dg-options "-O2 -ftree-vectorize -funsafe-math-optimizations -fno-math-errno -march=c86-4g-m7 -mprefer-vector-width=512 -mveclibabi=hvml" } */

/* Declare the scalar math functions directly so that this test does not
   depend on the target system's math headers.  */
float acosf (float);
float acoshf (float);
float acospif (float);
float asinf (float);
float asinhf (float);
float asinpif (float);
float atanf (float);
float atan2f (float, float);
float atan2pif (float, float);
float atanhf (float);
float atanpif (float);
float cbrtf (float);
float cosf (float);
float coshf (float);
float cospif (float);
float erff (float);
float erfcf (float);
float expf (float);
float exp2f (float);
float exp10f (float);
float expm1f (float);
float fdimf (float, float);
float fmodf (float, float);
float hypotf (float, float);
float logf (float);
float log1pf (float);
float log2f (float);
float log10f (float);
float logbf (float);
float nextafterf (float, float);
float powf (float, float);
float remainderf (float, float);
float sinf (float);
float sinhf (float);
float sinpif (float);
float tanf (float);
float tanhf (float);
float tanpif (float);
float tgammaf (float);

double acos (double);
double acosh (double);
double acospi (double);
double asin (double);
double asinh (double);
double asinpi (double);
double atan (double);
double atan2 (double, double);
double atan2pi (double, double);
double atanh (double);
double atanpi (double);
double cbrt (double);
double cos (double);
double cosh (double);
double cospi (double);
double erf (double);
double erfc (double);
double exp (double);
double exp2 (double);
double exp10 (double);
double expm1 (double);
double fdim (double, double);
double fmod (double, double);
double hypot (double, double);
double log (double);
double log1p (double);
double log2 (double);
double log10 (double);
double logb (double);
double nextafter (double, double);
double pow (double, double);
double remainder (double, double);
double sin (double);
double sinh (double);
double sinpi (double);
double tan (double);
double tanh (double);
double tanpi (double);
double tgamma (double);

#define gentest1(FUN, BASE, VF)                                       \
  extern BASE s_##FUN##_##BASE##_##VF[VF];                            \
  extern BASE d_##FUN##_##BASE##_##VF[VF];                            \
  void test_##FUN##_##BASE##_##VF (void)                              \
  {                                                                   \
    for (int i = 0; i < VF; i++)                                      \
      d_##FUN##_##BASE##_##VF[i] = FUN (s_##FUN##_##BASE##_##VF[i]);  \
  }

#define gentest2(FUN, BASE, VF)                                      \
  extern BASE s1_##FUN##_##BASE##_##VF[VF];                          \
  extern BASE s2_##FUN##_##BASE##_##VF[VF];                          \
  extern BASE d_##FUN##_##BASE##_##VF[VF];                           \
  void test_##FUN##_##BASE##_##VF (void)                             \
  {                                                                  \
    for (int i = 0; i < VF; i++)                                     \
      d_##FUN##_##BASE##_##VF[i]                                     \
        = FUN (s1_##FUN##_##BASE##_##VF[i],                          \
               s2_##FUN##_##BASE##_##VF[i]);                         \
  }

#define all_widths1(FUNF, FUND)        \
  gentest1 (FUNF, float, 4)            \
  gentest1 (FUNF, float, 8)            \
  gentest1 (FUNF, float, 16)           \
  gentest1 (FUND, double, 2)           \
  gentest1 (FUND, double, 4)           \
  gentest1 (FUND, double, 8)

#define all_widths2(FUNF, FUND)        \
  gentest2 (FUNF, float, 4)            \
  gentest2 (FUNF, float, 8)            \
  gentest2 (FUNF, float, 16)           \
  gentest2 (FUND, double, 2)           \
  gentest2 (FUND, double, 4)           \
  gentest2 (FUND, double, 8)

#define no_512_width1(FUNF, FUND)      \
  gentest1 (FUNF, float, 4)            \
  gentest1 (FUNF, float, 8)            \
  gentest1 (FUND, double, 2)           \
  gentest1 (FUND, double, 4)

#define no_512_width2(FUNF, FUND)      \
  gentest2 (FUNF, float, 4)            \
  gentest2 (FUNF, float, 8)            \
  gentest2 (FUND, double, 2)           \
  gentest2 (FUND, double, 4)

all_widths1 (asinf, asin)
all_widths1 (atanf, atan)
all_widths1 (atanhf, atanh)
all_widths1 (cbrtf, cbrt)
all_widths1 (cosf, cos)
all_widths1 (coshf, cosh)
all_widths1 (expf, exp)
all_widths1 (expm1f, expm1)
all_widths1 (logf, log)
all_widths1 (log1pf, log1p)
all_widths1 (log10f, log10)
all_widths1 (sinf, sin)
all_widths1 (sinhf, sinh)
all_widths1 (tanf, tan)
all_widths1 (tanhf, tanh)

all_widths2 (atan2f, atan2)
all_widths2 (powf, pow)

no_512_width1 (acoshf, acosh)
no_512_width1 (acospif, acospi)
no_512_width1 (asinhf, asinh)
no_512_width1 (asinpif, asinpi)
no_512_width2 (atan2pif, atan2pi)
no_512_width1 (atanpif, atanpi)
no_512_width1 (cospif, cospi)
no_512_width1 (erfcf, erfc)
no_512_width1 (exp10f, exp10)
no_512_width1 (logbf, logb)
no_512_width1 (sinpif, sinpi)
no_512_width1 (tanpif, tanpi)
no_512_width1 (tgammaf, tgamma)

no_512_width2 (fdimf, fdim)
no_512_width2 (hypotf, hypot)
no_512_width2 (nextafterf, nextafter)
no_512_width2 (remainderf, remainder)

gentest1 (acosf, float, 4)
gentest1 (acosf, float, 8)
gentest1 (acos, double, 2)
gentest1 (acos, double, 4)
gentest1 (acos, double, 8)

gentest1 (erff, float, 4)
gentest1 (erff, float, 16)
gentest1 (erf, double, 2)
gentest1 (erf, double, 4)

gentest1 (exp2f, float, 4)
gentest1 (exp2f, float, 8)
gentest1 (exp2, double, 2)
gentest1 (exp2, double, 4)
gentest1 (exp2, double, 8)

gentest1 (log2f, float, 4)
gentest1 (log2f, float, 8)
gentest1 (log2, double, 2)
gentest1 (log2, double, 4)
gentest1 (log2, double, 8)

gentest2 (fmodf, float, 4)
gentest2 (fmodf, float, 8)
gentest2 (fmod, double, 2)
gentest2 (fmod, double, 4)
gentest2 (fmod, double, 8)

/* Check every supported operation/precision/width combination separately.  */
/* { dg-final { scan-assembler-times {\mhvml_vs4_acos\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_acos\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_acos\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_acos\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_acos\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_acosh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_acosh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_acosh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_acosh\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_acospi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_acospi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_acospi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_acospi\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_asin\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_asin\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs16_asin\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_asin\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_asin\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_asin\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_asinh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_asinh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_asinh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_asinh\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_asinpi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_asinpi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_asinpi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_asinpi\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_atan\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_atan\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs16_atan\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_atan\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_atan\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_atan\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_atan2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_atan2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs16_atan2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_atan2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_atan2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_atan2\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_atan2pi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_atan2pi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_atan2pi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_atan2pi\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_atanh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_atanh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs16_atanh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_atanh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_atanh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_atanh\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_atanpi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_atanpi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_atanpi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_atanpi\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_cbrt\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_cbrt\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs16_cbrt\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_cbrt\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_cbrt\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_cbrt\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_cos\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_cos\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs16_cos\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_cos\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_cos\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_cos\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_cosh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_cosh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs16_cosh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_cosh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_cosh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_cosh\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_cospi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_cospi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_cospi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_cospi\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_erf\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs16_erf\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_erf\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_erf\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_erfc\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_erfc\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_erfc\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_erfc\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_exp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_exp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs16_exp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_exp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_exp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_exp\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_exp2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_exp2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_exp2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_exp2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_exp2\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_exp10\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_exp10\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_exp10\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_exp10\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_expm1\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_expm1\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs16_expm1\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_expm1\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_expm1\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_expm1\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_fdim\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_fdim\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_fdim\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_fdim\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_fmod\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_fmod\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_fmod\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_fmod\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_fmod\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_hypot\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_hypot\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_hypot\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_hypot\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_ln\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_ln\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs16_ln\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_ln\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_ln\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_ln\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_log1p\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_log1p\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs16_log1p\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_log1p\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_log1p\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_log1p\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_log2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_log2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_log2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_log2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_log2\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_log10\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_log10\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs16_log10\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_log10\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_log10\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_log10\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_logb\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_logb\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_logb\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_logb\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_nextafter\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_nextafter\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_nextafter\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_nextafter\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_pow\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_pow\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs16_pow\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_pow\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_pow\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_pow\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_remainder\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_remainder\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_remainder\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_remainder\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_sin\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_sin\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs16_sin\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_sin\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_sin\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_sin\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_sinh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_sinh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs16_sinh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_sinh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_sinh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_sinh\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_sinpi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_sinpi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_sinpi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_sinpi\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_tan\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_tan\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs16_tan\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_tan\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_tan\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_tan\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_tanh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_tanh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs16_tanh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_tanh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_tanh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd8_tanh\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_tanpi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_tanpi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_tanpi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_tanpi\M} 1 } } */

/* { dg-final { scan-assembler-times {\mhvml_vs4_tgamma\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vs8_tgamma\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd2_tgamma\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhvml_vd4_tgamma\M} 1 } } */
