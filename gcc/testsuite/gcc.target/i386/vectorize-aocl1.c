/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math -march=znver5 -mveclibabi=aocl" } */


/* Declare glibc math functions we need since this testcase may be run on
   systems that don't have glibc.  */
float tanf(float);
float expf(float);
float exp2f(float);
float logf(float);
float log2f(float);
float cosf(float);
float sinf(float);
float powf(float, float);
float erff(float);
float atanf(float);
float log10f(float);
float exp10f(float);
float expm1f(float);
float log1pf(float);
float asinf(float);
float acosf(float);
float tanhf(float);
float coshf(float);

double tan(double);
double exp(double);
double exp2(double);
double log(double);
double log2(double);
double cos(double);
double sin(double);
double pow(double, double);
double erf(double);
double atan(double);
double log10(double);
double exp10(double);
double expm1(double);
double log1p(double);
double asin(double);
double acos(double);
double tanh(double);
double cosh(double);

#define gentest1(FUN, BASE, VF)					\
  extern BASE s_##FUN##_##BASE##_##VF[VF];			\
  extern BASE d_##FUN##_##BASE##_##VF[VF];			\
  void test_##FUN##_##BASE##_##VF (void)			\
  {								\
    for (int i = 0; i < VF; i++)				\
      d_##FUN##_##BASE##_##VF[i]				\
	= FUN (s_##FUN##_##BASE##_##VF[i]);			\
  }								\


#define gentest2(FUN, BASE, VF)					\
  extern BASE s1_##FUN##_##BASE##_##VF[VF];			\
  extern BASE s2_##FUN##_##BASE##_##VF[VF];			\
  extern BASE d_##FUN##_##BASE##_##VF[VF];			\
  void test_##FUN##_##BASE##_##VF (void)			\
  {								\
    for (int i = 0; i < VF; i++)				\
      d_##FUN##_##BASE##_##VF[i]				\
	= FUN (s1_##FUN##_##BASE##_##VF[i],			\
	       s2_##FUN##_##BASE##_##VF[i]);			\
  }								\


gentest1(tan, float, 16)

#define COMMON_FLOAT_TESTS1(FUN)			\
  gentest1(FUN, float, 4)			\
  gentest1(FUN, float, 8)			\
  gentest1(FUN, float, 16)

COMMON_FLOAT_TESTS1(exp)
COMMON_FLOAT_TESTS1(exp2)
COMMON_FLOAT_TESTS1(log)
COMMON_FLOAT_TESTS1(log2)
COMMON_FLOAT_TESTS1(cos)
COMMON_FLOAT_TESTS1(sin)

gentest2(powf, float, 4)
gentest2(powf, float, 8)
gentest2(powf, float, 16)

//COMMON_FLOAT_TESTS1(sqrt)  provided by an instruction
COMMON_FLOAT_TESTS1(erf)

//gentest1(fabsf, float, 4)  provided by an instruction
//gentest1(fabsf, float, 8)  provided by an instruction

COMMON_FLOAT_TESTS1(atan)
COMMON_FLOAT_TESTS1(log10)

gentest1(exp10f, float, 4)
gentest1(expm1f, float, 4)
gentest1(log1pf, float, 4)

COMMON_FLOAT_TESTS1(asinf)

gentest1(acosf, float, 4)
gentest1(acosf, float, 16)

COMMON_FLOAT_TESTS1(tanhf)

gentest1(coshf, float, 4)
gentest1(coshf, float, 8)

#define COMMON_DOUBLE_TESTS1(FUN)			\
  gentest1(FUN, double, 2)			\
  gentest1(FUN, double, 4)			\
  gentest1(FUN, double, 8)


COMMON_DOUBLE_TESTS1(tan)
COMMON_DOUBLE_TESTS1(exp)
COMMON_DOUBLE_TESTS1(exp2)
COMMON_DOUBLE_TESTS1(log)
COMMON_DOUBLE_TESTS1(log2)
COMMON_DOUBLE_TESTS1(cos)
COMMON_DOUBLE_TESTS1(sin)

gentest2(pow, double, 2)
gentest2(pow, double, 4)
gentest2(pow, double, 8)

//COMMON_DOUBLE_TESTS1(sqrt)  provided by an instruction
COMMON_DOUBLE_TESTS1(erf)

//gentest1(fabs, double, 2)  provided by an instruction
//gentest1(fabs, double, 4)  provided by an instruction

gentest1(atan, double, 2)
gentest1(atan, double, 8)

gentest1(log10, double, 2)
gentest1(exp10, double, 2)
gentest1(expm1, double, 2)
gentest1(log1p, double, 2)

gentest1(asin, double, 8)


/* { dg-final { scan-assembler-times "amd_vrs8_expf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs8_exp2f" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs4_expf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs4_exp2f" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs4_exp10f" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs4_expm1f" 1 } } */
/* { dg-final { scan-assembler "amd_vrd2_exp" } } */
/* { dg-final { scan-assembler-times "amd_vrd2_exp2" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd2_exp10" 1 } } */
/* { dg-final { scan-assembler "amd_vrd4_exp" } } */
/* { dg-final { scan-assembler-times "amd_vrd4_exp2" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs16_expf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs16_exp2f" 1 } } */
/* { dg-final { scan-assembler "amd_vrd8_exp" } } */
/* { dg-final { scan-assembler-times "amd_vrd8_exp2" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs8_logf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs8_log2f" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs8_log10f" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs4_logf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs4_log2f" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs4_log10f" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs4_log1pf" 1 } } */
/* { dg-final { scan-assembler "amd_vrd4_log" } } */
/* { dg-final { scan-assembler-times "amd_vrd4_log2" 1 } } */
/* { dg-final { scan-assembler "amd_vrd2_log" } } */
/* { dg-final { scan-assembler-times "amd_vrd2_log2" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd2_log10" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd2_log1p" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs16_logf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs16_log2f" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs16_log10f" 1 } } */
/* { dg-final { scan-assembler "amd_vrd8_log" } } */
/* { dg-final { scan-assembler-times "amd_vrd8_log2" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs4_cosf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs8_cosf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs4_sinf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs8_sinf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd4_sin" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd4_cos" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd4_tan" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd2_cos" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd2_sin" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd2_tan" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs16_cosf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs16_sinf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs16_tanf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd8_cos" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd8_sin" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd8_tan" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs4_acosf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs4_asinf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs8_asinf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs4_atanf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs8_atanf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd2_atan" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs16_atanf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs16_asinf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs16_acosf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd8_atan" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd8_asin" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs4_coshf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs4_tanhf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs8_coshf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs8_tanhf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs16_tanhf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs4_powf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd2_pow" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd4_pow" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs8_powf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs16_powf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd8_pow" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs4_erff" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd2_erf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs8_erff" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd4_erf" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrs16_erff" 1 } } */
/* { dg-final { scan-assembler-times "amd_vrd8_erf" 1 } } */



