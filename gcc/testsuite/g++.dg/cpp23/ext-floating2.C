// P1467R9 - Extended floating-point types and standard names.
// { dg-do compile { target c++23 } }
// { dg-options "" }
// { dg-add-options float16 }
// { dg-add-options float32 }
// { dg-add-options float64 }
// { dg-add-options float128 }

#include "ext-floating.h"

#ifdef __STRICT_ANSI__
#undef __SIZEOF_FLOAT128__
#endif

using namespace std;

float fa = 1.0f;
float fb = (float) 1.0f;
float fc = 1.0;
float fd = (float) 1.0;
float fe = 1.0L;
float ff = (float) 1.0L;
#ifdef __SIZEOF_FLOAT128__
float fg = 1.0Q;
float fh = (float) 1.0Q;
#endif
double da = 1.0f;
double db = (double) 1.0f;
double dc = 1.0;
double dd = (double) 1.0;
double de = 1.0L;
double df = (double) 1.0L;
#ifdef __SIZEOF_FLOAT128__
double dg = 1.0Q;
double dh = (double) 1.0Q;
#endif
long double lda = 1.0f;
long double ldb = (long double) 1.0f;
long double ldc = 1.0;
long double ldd = (long double) 1.0;
long double lde = 1.0L;
long double ldf = (long double) 1.0L;
#ifdef __SIZEOF_FLOAT128__
long double ldg = 1.0Q;
long double ldh = (long double) 1.0Q;
__float128 qa = 1.0f;
__float128 qb = (__float128) 1.0f;
__float128 qc = 1.0;
__float128 qd = (__float128) 1.0;
__float128 qe = 1.0L;
__float128 qf = (__float128) 1.0L;
__float128 qg = 1.0Q;
__float128 qh = (__float128) 1.0Q;
#endif
#ifdef __STDCPP_FLOAT16_T__
float16_t f16a = 1.0F16;
float16_t f16b = (float16_t) 1.0F16;
#ifdef __STDCPP_FLOAT32_T__
float16_t f16c = 1.0F32;		// { dg-warning "converting to 'std::float16_t' \\\{aka '_Float16'\\\} from '_Float32' with greater conversion rank" "" { target { float16 && float32 } } }
float16_t f16d = (float16_t) 1.0F32;
#endif
#ifdef __STDCPP_FLOAT64_T__
float16_t f16e = 1.0F64;		// { dg-warning "converting to 'std::float16_t' \\\{aka '_Float16'\\\} from '_Float64' with greater conversion rank" "" { target { float16 && float64 } } }
float16_t f16f = (float16_t) 1.0F64;
#endif
#ifdef __STDCPP_FLOAT128_T__
float16_t f16g = 1.0F128;		// { dg-warning "converting to 'std::float16_t' \\\{aka '_Float16'\\\} from '_Float128' with greater conversion rank" "" { target { float16 && float128 } } }
float16_t f16h = (float16_t) 1.0F128;
#endif
float16_t f16j = (float16_t) 1.0f;
float16_t f16l = (float16_t) 1.0;
float16_t f16n = (float16_t) 1.0L;
#ifdef __SIZEOF_FLOAT128__
float16_t f16p = (float16_t) 1.0Q;
#endif
#endif
#ifdef __STDCPP_FLOAT32_T__
#ifdef __STDCPP_FLOAT16_T__
float32_t f32a = 1.0F16;
float32_t f32b = (float32_t) 1.0F16;
#endif
float32_t f32c = 1.0F32;
float32_t f32d = (float32_t) 1.0F32;
#ifdef __STDCPP_FLOAT64_T__
float32_t f32e = 1.0F64;		// { dg-warning "converting to 'std::float32_t' \\\{aka '_Float32'\\\} from '_Float64' with greater conversion rank" "" { target { float32 && float64 } } }
float32_t f32f = (float32_t) 1.0F64;
#endif
#ifdef __STDCPP_FLOAT128_T__
float32_t f32g = 1.0F128;		// { dg-warning "converting to 'std::float32_t' \\\{aka '_Float32'\\\} from '_Float128' with greater conversion rank" "" { target { float32 && float128 } } }
float32_t f32h = (float32_t) 1.0F128;
#endif
#if __FLT_MAX_EXP__ <= __FLT32_MAX_EXP__ && __FLT_MANT_DIG__ <= __FLT32_MANT_DIG__
float32_t f32i = 1.0f;
#endif
float32_t f32j = (float32_t) 1.0f;
float32_t f32l = (float32_t) 1.0;
float32_t f32n = (float32_t) 1.0L;
#ifdef __SIZEOF_FLOAT128__
float32_t f32p = (float32_t) 1.0Q;
#endif
#endif
#ifdef __STDCPP_FLOAT64_T__
#ifdef __STDCPP_FLOAT16_T__
float64_t f64a = 1.0F16;
float64_t f64b = (float64_t) 1.0F16;
#endif
#ifdef __STDCPP_FLOAT32_T__
float64_t f64c = 1.0F32;
float64_t f64d = (float64_t) 1.0F32;
#endif
float64_t f64e = 1.0F64;
float64_t f64f = (float64_t) 1.0F64;
#ifdef __STDCPP_FLOAT128_T__
float64_t f64g = 1.0F128;		// { dg-warning "converting to 'std::float64_t' \\\{aka '_Float64'\\\} from '_Float128' with greater conversion rank" "" { target { float64 && float128 } } }
float64_t f64h = (float64_t) 1.0F128;
#endif
#if __FLT_MAX_EXP__ <= __FLT64_MAX_EXP__ && __FLT_MANT_DIG__ <= __FLT64_MANT_DIG__
float64_t f64i = 1.0f;
#endif
float64_t f64j = (float64_t) 1.0f;
#if __DBL_MAX_EXP__ <= __FLT64_MAX_EXP__ && __DBL_MANT_DIG__ <= __FLT64_MANT_DIG__
float64_t f64k = 1.0;
#endif
float64_t f64l = (float64_t) 1.0;
float64_t f64n = (float64_t) 1.0L;
#ifdef __SIZEOF_FLOAT128__
float64_t f64p = (float64_t) 1.0Q;
#endif
#endif
#ifdef __STDCPP_FLOAT128_T__
#ifdef __STDCPP_FLOAT16_T__
float128_t f128a = 1.0F16;
float128_t f128b = (float128_t) 1.0F16;
#endif
#ifdef __STDCPP_FLOAT32_T__
float128_t f128c = 1.0F32;
float128_t f128d = (float128_t) 1.0F32;
#endif
#ifdef __STDCPP_FLOAT64_T__
float128_t f128e = 1.0F64;
float128_t f128f = (float128_t) 1.0F64;
#endif
float128_t f128g = 1.0F128;
float128_t f128h = (float128_t) 1.0F128;
#if __FLT_MAX_EXP__ <= __FLT128_MAX_EXP__ && __FLT_MANT_DIG__ <= __FLT128_MANT_DIG__
float128_t f128i = 1.0f;
#endif
float128_t f128j = (float128_t) 1.0f;
#if __DBL_MAX_EXP__ <= __FLT128_MAX_EXP__ && __DBL_MANT_DIG__ <= __FLT128_MANT_DIG__
float128_t f128k = 1.0;
#endif
float128_t f128l = (float128_t) 1.0;
#if __LDBL_MAX_EXP__ <= __FLT128_MAX_EXP__ && __LDBL_MANT_DIG__ <= __FLT128_MANT_DIG__ && __LDBL_MANT_DIG__ != 106
float128_t f128m = 1.0L;
#endif
float128_t f128n = (float128_t) 1.0L;
#ifdef __SIZEOF_FLOAT128__
float128_t f128o = 1.0Q;
float128_t f128p = (float128_t) 1.0Q;
#endif
#endif
