/* PR tree-optimization/113993 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-add-options float32 } */
/* { dg-add-options float64 } */
/* { dg-add-options float128 } */
/* { dg-add-options float32x } */
/* { dg-add-options float64x } */
/* { dg-final { scan-tree-dump-not "__builtin_\[a-z0-9\] \\\(\[^\n\r\]\\\);" "optimized" } } */

void
flt (float f1, float f2, float f3, float f4, float f5,
     float f6, float f7, float f8, float f9, float f10)
{
  if (!(f1 >= -1.0f && f1 <= 1.0f)) __builtin_unreachable ();
  __builtin_acosf (f1);
  __builtin_asinf (f1);
  if (!(f2 >= 1.0f && f2 <= __builtin_inff ())) __builtin_unreachable ();
  __builtin_acoshf (f2);
  if (!(f3 > -1.0f && f3 < 1.0f)) __builtin_unreachable ();
  __builtin_atanhf (f3);
  if (!(f4 > 0.0f && f4 < __builtin_inff ())) __builtin_unreachable ();
  __builtin_logf (f4);
  __builtin_log2f (f4);
  __builtin_log10f (f4);
  if (!(f5 > -1.0f && f5 < __builtin_inff ())) __builtin_unreachable ();
  __builtin_log1pf (f5);
  if (!(f6 >= 0.0f && f6 < __builtin_inff ())) __builtin_unreachable ();
  __builtin_sqrtf (f6);
#if __FLT_MANT_DIG__ == __FLT32_MANT_DIG__ && __FLT_MAX_EXP__ == __FLT32_MAX_EXP__
  if (!(f7 > -89.0f && f7 < 89.0f)) __builtin_unreachable ();
  __builtin_coshf (f7);
  __builtin_sinhf (f7);
  if (!(f8 > -__builtin_inff () && f8 < 88.0f)) __builtin_unreachable ();
  __builtin_expf (f8);
  if (!(f9 > -__builtin_inff () && f9 < 128.0f)) __builtin_unreachable ();
  __builtin_exp2f (f9);
  if (!(f10 > -__builtin_inff () && f10 < 38.0f)) __builtin_unreachable ();
  __builtin_exp10f (f10);
#endif
}

#if defined(__FLT16_MANT_DIG__) && 0 /* No library routines here, these don't actually fold away.  */
void
flt16 (_Float16 f1, _Float16 f2, _Float16 f3, _Float16 f4, _Float16 f5,
       _Float16 f6, _Float16 f7, _Float16 f8, _Float16 f9)
{
  if (!(f1 >= -1.0f16 && f1 <= 1.0f16)) __builtin_unreachable ();
  __builtin_acosf16 (f1);
  __builtin_asinf16 (f1);
  if (!(f2 >= 1.0f16 && f2 <= __builtin_inff16 ())) __builtin_unreachable ();
  __builtin_acoshf16 (f2);
  if (!(f3 > -1.0f16 && f3 < 1.0f16)) __builtin_unreachable ();
  __builtin_atanhf16 (f3);
  if (!(f4 > 0.0f16 && f4 < __builtin_inff16 ())) __builtin_unreachable ();
  __builtin_logf16 (f4);
  __builtin_log2f16 (f4);
  __builtin_log10f16 (f4);
  if (!(f5 > -1.0f16 && f5 < __builtin_inff16 ())) __builtin_unreachable ();
  __builtin_log1pf16 (f5);
  if (!(f6 >= 0.0f16 && f6 < __builtin_inff16 ())) __builtin_unreachable ();
  __builtin_sqrtf16 (f6);
  if (!(f7 > -11.0f16 && f7 < 11.0f16)) __builtin_unreachable ();
  __builtin_coshf16 (f7);
  __builtin_sinhf16 (f7);
  if (!(f8 > -__builtin_inff16 () && f8 < 11.0f16)) __builtin_unreachable ();
  __builtin_expf16 (f8);
  if (!(f9 > -__builtin_inff16 () && f9 < 16.0f16)) __builtin_unreachable ();
  __builtin_exp2f16 (f9);
}
#endif

#ifdef __FLT32_MANT_DIG__
void
flt32 (_Float32 f1, _Float32 f2, _Float32 f3, _Float32 f4, _Float32 f5,
       _Float32 f6, _Float32 f7, _Float32 f8, _Float32 f9)
{
  if (!(f1 >= -1.0f32 && f1 <= 1.0f32)) __builtin_unreachable ();
  __builtin_acosf32 (f1);
  __builtin_asinf32 (f1);
  if (!(f2 >= 1.0f32 && f2 <= __builtin_inff32 ())) __builtin_unreachable ();
  __builtin_acoshf32 (f2);
  if (!(f3 > -1.0f32 && f3 < 1.0f32)) __builtin_unreachable ();
  __builtin_atanhf32 (f3);
  if (!(f4 > 0.0f32 && f4 < __builtin_inff32 ())) __builtin_unreachable ();
  __builtin_logf32 (f4);
  __builtin_log2f32 (f4);
  __builtin_log10f32 (f4);
  if (!(f5 > -1.0f32 && f5 < __builtin_inff32 ())) __builtin_unreachable ();
  __builtin_log1pf32 (f5);
  if (!(f6 >= 0.0f32 && f6 < __builtin_inff32 ())) __builtin_unreachable ();
  __builtin_sqrtf32 (f6);
  if (!(f7 > -89.0f32 && f7 < 89.0f32)) __builtin_unreachable ();
  __builtin_coshf32 (f7);
  __builtin_sinhf32 (f7);
  if (!(f8 > -__builtin_inff32 () && f8 < 88.0f32)) __builtin_unreachable ();
  __builtin_expf32 (f8);
  if (!(f9 > -__builtin_inff32 () && f9 < 128.0f32)) __builtin_unreachable ();
  __builtin_exp2f32 (f9);
}
#endif

void
dbl (double f1, double f2, double f3, double f4, double f5,
     double f6, double f7, double f8, double f9, double f10)
{
  if (!(f1 >= -1.0 && f1 <= 1.0)) __builtin_unreachable ();
  __builtin_acos (f1);
  __builtin_asin (f1);
  if (!(f2 >= 1.0 && f2 <= __builtin_inf ())) __builtin_unreachable ();
  __builtin_acosh (f2);
  if (!(f3 > -1.0 && f3 < 1.0)) __builtin_unreachable ();
  __builtin_atanh (f3);
  if (!(f4 > 0.0 && f4 < __builtin_inf ())) __builtin_unreachable ();
  __builtin_log (f4);
  __builtin_log2 (f4);
  __builtin_log10 (f4);
  if (!(f5 > -1.0 && f5 < __builtin_inf ())) __builtin_unreachable ();
  __builtin_log1p (f5);
  if (!(f6 >= 0.0 && f6 < __builtin_inf ())) __builtin_unreachable ();
  __builtin_sqrt (f6);
#if __DBL_MANT_DIG__ == __FLT64_MANT_DIG__ && __DBL_MAX_EXP__ == __FLT64_MAX_EXP__
  if (!(f7 > -710.0 && f7 < 710.0)) __builtin_unreachable ();
  __builtin_cosh (f7);
  __builtin_sinh (f7);
  if (!(f8 > -__builtin_inf () && f8 < 709.0)) __builtin_unreachable ();
  __builtin_exp (f8);
  if (!(f9 > -__builtin_inf () && f9 < 1024.0)) __builtin_unreachable ();
  __builtin_exp2 (f9);
  if (!(f10 > -__builtin_inf () && f10 < 308.0)) __builtin_unreachable ();
  __builtin_exp10 (f10);
#endif
}

#ifdef __FLT64_MANT_DIG__
void
flt64 (_Float64 f1, _Float64 f2, _Float64 f3, _Float64 f4, _Float64 f5,
       _Float64 f6, _Float64 f7, _Float64 f8, _Float64 f9)
{
  if (!(f1 >= -1.0f64 && f1 <= 1.0f64)) __builtin_unreachable ();
  __builtin_acosf64 (f1);
  __builtin_asinf64 (f1);
  if (!(f2 >= 1.0f64 && f2 <= __builtin_inff64 ())) __builtin_unreachable ();
  __builtin_acoshf64 (f2);
  if (!(f3 > -1.0f64 && f3 < 1.0f64)) __builtin_unreachable ();
  __builtin_atanhf64 (f3);
  if (!(f4 > 0.0f64 && f4 < __builtin_inff64 ())) __builtin_unreachable ();
  __builtin_logf64 (f4);
  __builtin_log2f64 (f4);
  __builtin_log10f64 (f4);
  if (!(f5 > -1.0f64 && f5 < __builtin_inff64 ())) __builtin_unreachable ();
  __builtin_log1pf64 (f5);
  if (!(f6 >= 0.0f64 && f6 < __builtin_inff64 ())) __builtin_unreachable ();
  __builtin_sqrtf64 (f6);
  if (!(f7 > -710.0f64 && f7 < 710.0f64)) __builtin_unreachable ();
  __builtin_coshf64 (f7);
  __builtin_sinhf64 (f7);
  if (!(f8 > -__builtin_inff64 () && f8 < 709.0f64)) __builtin_unreachable ();
  __builtin_expf64 (f8);
  if (!(f9 > -__builtin_inff64 () && f9 < 1024.0f64)) __builtin_unreachable ();
  __builtin_exp2f64 (f9);
}
#endif

#ifdef __FLT32X_MANT_DIG__
void
flt32x (_Float32x f1, _Float32x f2, _Float32x f3, _Float32x f4, _Float32x f5,
	_Float32x f6, _Float32x f7, _Float32x f8, _Float32x f9)
{
  if (!(f1 >= -1.0f32x && f1 <= 1.0f32x)) __builtin_unreachable ();
  __builtin_acosf32x (f1);
  __builtin_asinf32x (f1);
  if (!(f2 >= 1.0f32x && f2 <= __builtin_inff32x ())) __builtin_unreachable ();
  __builtin_acoshf32x (f2);
  if (!(f3 > -1.0f32x && f3 < 1.0f32x)) __builtin_unreachable ();
  __builtin_atanhf32x (f3);
  if (!(f4 > 0.0f32x && f4 < __builtin_inff32x ())) __builtin_unreachable ();
  __builtin_logf32x (f4);
  __builtin_log2f32x (f4);
  __builtin_log10f32x (f4);
  if (!(f5 > -1.0f32x && f5 < __builtin_inff32x ())) __builtin_unreachable ();
  __builtin_log1pf32x (f5);
  if (!(f6 >= 0.0f32x && f6 < __builtin_inff32x ())) __builtin_unreachable ();
  __builtin_sqrtf32x (f6);
#if __FLT32X_MANT_DIG__ == __FLT64_MANT_DIG__ && __FLT32X_MAX_EXP__ == __FLT64_MAX_EXP__
  if (!(f7 > -710.0f32x && f7 < 710.0f32x)) __builtin_unreachable ();
  __builtin_coshf32x (f7);
  __builtin_sinhf32x (f7);
  if (!(f8 > -__builtin_inff32x () && f8 < 709.0f32x)) __builtin_unreachable ();
  __builtin_expf32x (f8);
  if (!(f9 > -__builtin_inff32x () && f9 < 1024.0f32x)) __builtin_unreachable ();
  __builtin_exp2f32x (f9);
#endif
}
#endif

void
ldbl (long double f1, long double f2, long double f3, long double f4, long double f5,
      long double f6, long double f7, long double f8, long double f9, long double f10)
{
  if (!(f1 >= -1.0L && f1 <= 1.0L)) __builtin_unreachable ();
  __builtin_acosl (f1);
  __builtin_asinl (f1);
  if (!(f2 >= 1.0L && f2 <= __builtin_infl ())) __builtin_unreachable ();
  __builtin_acoshl (f2);
  if (!(f3 > -1.0L && f3 < 1.0L)) __builtin_unreachable ();
  __builtin_atanhl (f3);
  if (!(f4 > 0.0L && f4 < __builtin_infl ())) __builtin_unreachable ();
  __builtin_logl (f4);
  __builtin_log2l (f4);
  __builtin_log10l (f4);
  if (!(f5 > -1.0L && f5 < __builtin_infl ())) __builtin_unreachable ();
  __builtin_log1pl (f5);
  if (!(f6 >= 0.0L && f6 < __builtin_infl ())) __builtin_unreachable ();
  __builtin_sqrtl (f6);
#if __LDBL_MAX_EXP__ == 16384
  if (!(f7 > -11357.0L && f7 < 11357.0L)) __builtin_unreachable ();
  __builtin_coshl (f7);
  __builtin_sinhl (f7);
  if (!(f8 > -__builtin_infl () && f8 < 11356.0L)) __builtin_unreachable ();
  __builtin_expl (f8);
  if (!(f9 > -__builtin_infl () && f9 < 16384.0L)) __builtin_unreachable ();
  __builtin_exp2l (f9);
  if (!(f10 > -__builtin_infl () && f10 < 4932.0L)) __builtin_unreachable ();
  __builtin_exp10l (f10);
#elif __LDBL_MANT_DIG__ == __FLT64_MANT_DIG__ && __LDBL_MAX_EXP__ == __FLT64_MAX_EXP__
  if (!(f7 > -710.0L && f7 < 710.0L)) __builtin_unreachable ();
  __builtin_coshl (f7);
  __builtin_sinhl (f7);
  if (!(f8 > -__builtin_infl () && f8 < 709.0L)) __builtin_unreachable ();
  __builtin_expl (f8);
  if (!(f9 > -__builtin_infl () && f9 < 1024.0L)) __builtin_unreachable ();
  __builtin_exp2l (f9);
  if (!(f10 > -__builtin_infl () && f10 < 308.0L)) __builtin_unreachable ();
  __builtin_exp10l (f10);
#endif
}

#ifdef __FLT128_MANT_DIG__
void
flt128 (_Float128 f1, _Float128 f2, _Float128 f3, _Float128 f4, _Float128 f5,
	_Float128 f6, _Float128 f7, _Float128 f8, _Float128 f9)
{
  if (!(f1 >= -1.0f128 && f1 <= 1.0f128)) __builtin_unreachable ();
  __builtin_acosf128 (f1);
  __builtin_asinf128 (f1);
  if (!(f2 >= 1.0f128 && f2 <= __builtin_inff128 ())) __builtin_unreachable ();
  __builtin_acoshf128 (f2);
  if (!(f3 > -1.0f128 && f3 < 1.0f128)) __builtin_unreachable ();
  __builtin_atanhf128 (f3);
  if (!(f4 > 0.0f128 && f4 < __builtin_inff128 ())) __builtin_unreachable ();
  __builtin_logf128 (f4);
  __builtin_log2f128 (f4);
  __builtin_log10f128 (f4);
  if (!(f5 > -1.0f128 && f5 < __builtin_inff128 ())) __builtin_unreachable ();
  __builtin_log1pf128 (f5);
  if (!(f6 >= 0.0f128 && f6 < __builtin_inff128 ())) __builtin_unreachable ();
  __builtin_sqrtf128 (f6);
  if (!(f7 > -11357.0f128 && f7 < 11357.0f128)) __builtin_unreachable ();
  __builtin_coshf128 (f7);
  __builtin_sinhf128 (f7);
  if (!(f8 > -__builtin_inff128 () && f8 < 11356.0f128)) __builtin_unreachable ();
  __builtin_expf128 (f8);
  if (!(f9 > -__builtin_inff128 () && f9 < 16384.0f128)) __builtin_unreachable ();
  __builtin_exp2f128 (f9);
}
#endif

#ifdef __FLT64X_MANT_DIG__
void
flt64x (_Float64x f1, _Float64x f2, _Float64x f3, _Float64x f4, _Float64x f5,
	_Float64x f6, _Float64x f7, _Float64x f8, _Float64x f9)
{
  if (!(f1 >= -1.0f64x && f1 <= 1.0f64x)) __builtin_unreachable ();
  __builtin_acosf64x (f1);
  __builtin_asinf64x (f1);
  if (!(f2 >= 1.0f64x && f2 <= __builtin_inff64x ())) __builtin_unreachable ();
  __builtin_acoshf64x (f2);
  if (!(f3 > -1.0f64x && f3 < 1.0f64x)) __builtin_unreachable ();
  __builtin_atanhf64x (f3);
  if (!(f4 > 0.0f64x && f4 < __builtin_inff64x ())) __builtin_unreachable ();
  __builtin_logf64x (f4);
  __builtin_log2f64x (f4);
  __builtin_log10f64x (f4);
  if (!(f5 > -1.0f64x && f5 < __builtin_inff64x ())) __builtin_unreachable ();
  __builtin_log1pf64x (f5);
  if (!(f6 >= 0.0f64x && f6 < __builtin_inff64x ())) __builtin_unreachable ();
  __builtin_sqrtf64x (f6);
#if __FLT64X_MAX_EXP__ == 16384
  if (!(f7 > -11357.0f64x && f7 < 11357.0f64x)) __builtin_unreachable ();
  __builtin_coshf64x (f7);
  __builtin_sinhf64x (f7);
  if (!(f8 > -__builtin_inff64x () && f8 < 11356.0f64x)) __builtin_unreachable ();
  __builtin_expf64x (f8);
  if (!(f9 > -__builtin_inff64x () && f9 < 16384.0f64x)) __builtin_unreachable ();
  __builtin_exp2f64x (f9);
#endif
}
#endif
