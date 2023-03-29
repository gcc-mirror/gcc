// P1467R9 - Extended floating-point types and standard names.
// { dg-do compile { target c++23 } }
// { dg-options "" }

#include "ext-floating.h"

#ifdef __STRICT_ANSI__
#undef __SIZEOF_FLOAT128__
#endif

using namespace std;

static_assert (!is_same<float, double>::value);
static_assert (!is_same<float, long double>::value);
static_assert (!is_same<double, long double>::value);
static_assert (is_same<decltype (0.0f), float>::value);
static_assert (is_same<decltype (0.0F), float>::value);
static_assert (is_same<decltype (0.0), double>::value);
static_assert (is_same<decltype (0.0l), long double>::value);
static_assert (is_same<decltype (0.0L), long double>::value);
static_assert (is_same<decltype (0.0f + 0.0F), float>::value);
static_assert (is_same<decltype (0.0F + 0.0f), float>::value);
static_assert (is_same<decltype (0.0 + 0.0), double>::value);
static_assert (is_same<decltype (0.0l + 0.0L), long double>::value);
static_assert (is_same<decltype (0.0L + 0.0l), long double>::value);
#ifdef __SIZEOF_FLOAT128__
static_assert (is_same<decltype (0.0q), __float128>::value);
static_assert (is_same<decltype (0.0Q), __float128>::value);
static_assert (is_same<decltype (0.0q + 0.0q), __float128>::value);
static_assert (is_same<decltype (0.0Q + 0.0Q), __float128>::value);
#endif
#ifdef __STDCPP_FLOAT16_T__
static_assert (!is_same<float, float16_t>::value);
static_assert (!is_same<double, float16_t>::value);
static_assert (!is_same<long double, float16_t>::value);
static_assert (is_same<decltype (0.0f16), float16_t>::value);
static_assert (is_same<decltype (0.0F16), float16_t>::value);
static_assert (is_same<decltype (0.0f16 + 0.0f16), float16_t>::value);
static_assert (is_same<decltype (0.0F16 + 0.0F16), float16_t>::value);
#endif
#ifdef __STDCPP_FLOAT32_T__
static_assert (!is_same<float, float32_t>::value);
static_assert (!is_same<double, float32_t>::value);
static_assert (!is_same<long double, float32_t>::value);
static_assert (!is_same<decltype (0.0f), float32_t>::value);
static_assert (!is_same<decltype (0.0F), float32_t>::value);
static_assert (is_same<decltype (0.0f32), float32_t>::value);
static_assert (is_same<decltype (0.0F32), float32_t>::value);
static_assert (!is_same<decltype (0.0f32), float>::value);
static_assert (!is_same<decltype (0.0F32), float>::value);
static_assert (is_same<decltype (0.0f32 + 0.0f32), float32_t>::value);
static_assert (is_same<decltype (0.0F32 + 0.0F32), float32_t>::value);
#endif
#ifdef __STDCPP_FLOAT64_T__
static_assert (!is_same<float, float64_t>::value);
static_assert (!is_same<double, float64_t>::value);
static_assert (!is_same<long double, float64_t>::value);
static_assert (!is_same<decltype (0.0), float64_t>::value);
static_assert (is_same<decltype (0.0f64), float64_t>::value);
static_assert (is_same<decltype (0.0F64), float64_t>::value);
static_assert (!is_same<decltype (0.0f64), double>::value);
static_assert (!is_same<decltype (0.0F64), double>::value);
static_assert (is_same<decltype (0.0f64 + 0.0f64), float64_t>::value);
static_assert (is_same<decltype (0.0F64 + 0.0F64), float64_t>::value);
#endif
#ifdef __STDCPP_FLOAT128_T__
static_assert (!is_same<float, float128_t>::value);
static_assert (!is_same<double, float128_t>::value);
static_assert (!is_same<long double, float128_t>::value);
static_assert (!is_same<decltype (0.0l), float128_t>::value);
static_assert (!is_same<decltype (0.0L), float128_t>::value);
static_assert (is_same<decltype (0.0f128), float128_t>::value);
static_assert (is_same<decltype (0.0F128), float128_t>::value);
static_assert (!is_same<decltype (0.0f128), long double>::value);
static_assert (!is_same<decltype (0.0F128), long double>::value);
static_assert (is_same<decltype (0.0f128 + 0.0f128), float128_t>::value);
static_assert (is_same<decltype (0.0F128 + 0.0F128), float128_t>::value);
#ifdef __SIZEOF_FLOAT128__
static_assert (!is_same<float128_t, __float128>::value);
static_assert (!is_same<decltype (0.0q), float128_t>::value);
static_assert (!is_same<decltype (0.0Q), float128_t>::value);
static_assert (!is_same<decltype (0.0f128), __float128>::value);
static_assert (!is_same<decltype (0.0F128), __float128>::value);
#endif
#endif
#ifdef __STDCPP_BFLOAT16_T__
static_assert (!is_same<float, bfloat16_t>::value);
static_assert (!is_same<double, bfloat16_t>::value);
static_assert (!is_same<long double, bfloat16_t>::value);
static_assert (is_same<decltype (0.0bf16), bfloat16_t>::value);
static_assert (is_same<decltype (0.0BF16), bfloat16_t>::value);
static_assert (is_same<decltype (0.0bf16 + 0.0bf16), bfloat16_t>::value);
static_assert (is_same<decltype (0.0BF16 + 0.0BF16), bfloat16_t>::value);
#endif
#ifdef __FLT32X_MANT_DIG__
static_assert (!is_same<float, _Float32x>::value);
static_assert (!is_same<double, _Float32x>::value);
static_assert (!is_same<long double, _Float32x>::value);
static_assert (!is_same<decltype (0.0f), _Float32x>::value);
static_assert (!is_same<decltype (0.0F), _Float32x>::value);
static_assert (is_same<decltype (0.0f32x), _Float32x>::value);
static_assert (is_same<decltype (0.0F32x), _Float32x>::value);
static_assert (!is_same<decltype (0.0f32x), float>::value);
static_assert (!is_same<decltype (0.0F32x), float>::value);
static_assert (is_same<decltype (0.0f32x + 0.0f32x), _Float32x>::value);
static_assert (is_same<decltype (0.0F32x + 0.0F32x), _Float32x>::value);
#ifdef __STDCPP_FLOAT16_T__
static_assert (!is_same<float16_t, _Float32x>::value);
static_assert (!is_same<decltype (0.0f16), _Float32x>::value);
static_assert (!is_same<decltype (0.0F16), _Float32x>::value);
static_assert (!is_same<decltype (0.0f32x), float16_t>::value);
static_assert (!is_same<decltype (0.0F32x), float16_t>::value);
#endif
#ifdef __STDCPP_FLOAT32_T__
static_assert (!is_same<float32_t, _Float32x>::value);
static_assert (!is_same<decltype (0.0f32), _Float32x>::value);
static_assert (!is_same<decltype (0.0F32), _Float32x>::value);
static_assert (!is_same<decltype (0.0f32x), float32_t>::value);
static_assert (!is_same<decltype (0.0F32x), float32_t>::value);
#endif
#ifdef __STDCPP_FLOAT64_T__
static_assert (!is_same<float64_t, _Float32x>::value);
static_assert (!is_same<decltype (0.0f64), _Float32x>::value);
static_assert (!is_same<decltype (0.0F64), _Float32x>::value);
static_assert (!is_same<decltype (0.0f32x), float64_t>::value);
static_assert (!is_same<decltype (0.0F32x), float64_t>::value);
#endif
#ifdef __STDCPP_FLOAT128_T__
static_assert (!is_same<float128_t, _Float32x>::value);
static_assert (!is_same<decltype (0.0f128), _Float32x>::value);
static_assert (!is_same<decltype (0.0F128), _Float32x>::value);
static_assert (!is_same<decltype (0.0f32x), float128_t>::value);
static_assert (!is_same<decltype (0.0F32x), float128_t>::value);
#endif
#endif
#ifdef __FLT64X_MANT_DIG__
static_assert (!is_same<float, _Float64x>::value);
static_assert (!is_same<double, _Float64x>::value);
static_assert (!is_same<long double, _Float64x>::value);
static_assert (!is_same<decltype (0.0), _Float64x>::value);
static_assert (is_same<decltype (0.0f64x), _Float64x>::value);
static_assert (is_same<decltype (0.0F64x), _Float64x>::value);
static_assert (!is_same<decltype (0.0f64x), double>::value);
static_assert (!is_same<decltype (0.0F64x), double>::value);
static_assert (is_same<decltype (0.0f64x + 0.0f64x), _Float64x>::value);
static_assert (is_same<decltype (0.0F64x + 0.0F64x), _Float64x>::value);
#ifdef __STDCPP_FLOAT16_T__
static_assert (!is_same<float16_t, _Float64x>::value);
static_assert (!is_same<decltype (0.0f16), _Float64x>::value);
static_assert (!is_same<decltype (0.0F16), _Float64x>::value);
static_assert (!is_same<decltype (0.0f64x), float16_t>::value);
static_assert (!is_same<decltype (0.0F64x), float16_t>::value);
#endif
#ifdef __STDCPP_FLOAT32_T__
static_assert (!is_same<float32_t, _Float64x>::value);
static_assert (!is_same<decltype (0.0f32), _Float64x>::value);
static_assert (!is_same<decltype (0.0F32), _Float64x>::value);
static_assert (!is_same<decltype (0.0f64x), float32_t>::value);
static_assert (!is_same<decltype (0.0F64x), float32_t>::value);
#endif
#ifdef __STDCPP_FLOAT64_T__
static_assert (!is_same<float64_t, _Float64x>::value);
static_assert (!is_same<decltype (0.0f64), _Float64x>::value);
static_assert (!is_same<decltype (0.0F64), _Float64x>::value);
static_assert (!is_same<decltype (0.0f64x), float64_t>::value);
static_assert (!is_same<decltype (0.0F64x), float64_t>::value);
#endif
#ifdef __STDCPP_FLOAT128_T__
static_assert (!is_same<float128_t, _Float64x>::value);
static_assert (!is_same<decltype (0.0f128), _Float64x>::value);
static_assert (!is_same<decltype (0.0F128), _Float64x>::value);
static_assert (!is_same<decltype (0.0f64x), float128_t>::value);
static_assert (!is_same<decltype (0.0F64x), float128_t>::value);
#endif
#ifdef __SIZEOF_FLOAT128__
static_assert (!is_same<_Float64x, __float128>::value);
static_assert (!is_same<decltype (0.0q), _Float64x>::value);
static_assert (!is_same<decltype (0.0Q), _Float64x>::value);
static_assert (!is_same<decltype (0.0f64x), __float128>::value);
static_assert (!is_same<decltype (0.0F64x), __float128>::value);
#endif
#endif
#ifdef __FLT128X_MANT_DIG__
static_assert (!is_same<float, _Float128x>::value);
static_assert (!is_same<double, _Float128x>::value);
static_assert (!is_same<long double, _Float128x>::value);
static_assert (!is_same<decltype (0.0l), _Float128x>::value);
static_assert (!is_same<decltype (0.0L), _Float128x>::value);
static_assert (is_same<decltype (0.0f128x), _Float128x>::value);
static_assert (is_same<decltype (0.0F128x), _Float128x>::value);
static_assert (!is_same<decltype (0.0f128x), long double>::value);
static_assert (!is_same<decltype (0.0F128x), long double>::value);
static_assert (is_same<decltype (0.0f128x + 0.0f128x), _Float128x>::value);
static_assert (is_same<decltype (0.0F128x + 0.0F128x), _Float128x>::value);
#ifdef __STDCPP_FLOAT16_T__
static_assert (!is_same<float16_t, _Float128x>::value);
static_assert (!is_same<decltype (0.0f16), _Float128x>::value);
static_assert (!is_same<decltype (0.0F16), _Float128x>::value);
static_assert (!is_same<decltype (0.0f128x), float16_t>::value);
static_assert (!is_same<decltype (0.0F128x), float16_t>::value);
#endif
#ifdef __STDCPP_FLOAT32_T__
static_assert (!is_same<float32_t, _Float128x>::value);
static_assert (!is_same<decltype (0.0f32), _Float128x>::value);
static_assert (!is_same<decltype (0.0F32), _Float128x>::value);
static_assert (!is_same<decltype (0.0f128x), float32_t>::value);
static_assert (!is_same<decltype (0.0F128x), float32_t>::value);
#endif
#ifdef __STDCPP_FLOAT64_T__
static_assert (!is_same<float64_t, _Float128x>::value);
static_assert (!is_same<decltype (0.0f64), _Float128x>::value);
static_assert (!is_same<decltype (0.0F64), _Float128x>::value);
static_assert (!is_same<decltype (0.0f128x), float64_t>::value);
static_assert (!is_same<decltype (0.0F128x), float64_t>::value);
#endif
#ifdef __STDCPP_FLOAT128_T__
static_assert (!is_same<float128_t, _Float128x>::value);
static_assert (!is_same<decltype (0.0f128), _Float128x>::value);
static_assert (!is_same<decltype (0.0F128), _Float128x>::value);
static_assert (!is_same<decltype (0.0f128x), float128_t>::value);
static_assert (!is_same<decltype (0.0F128x), float128_t>::value);
#endif
#ifdef __SIZEOF_FLOAT128__
static_assert (!is_same<_Float128x, __float128>::value);
static_assert (!is_same<decltype (0.0q), _Float128x>::value);
static_assert (!is_same<decltype (0.0Q), _Float128x>::value);
static_assert (!is_same<decltype (0.0f128x), __float128>::value);
static_assert (!is_same<decltype (0.0F128x), __float128>::value);
#endif
#endif
static_assert (is_same<decltype (0.0f + 0.0), double>::value);
static_assert (is_same<decltype (0.0 + 0.0F), double>::value);
static_assert (is_same<decltype (0.0L + 0.0), long double>::value);
static_assert (is_same<decltype (0.0 + 0.0L), long double>::value);
static_assert (is_same<decltype (0.0L + 0.0f), long double>::value);
static_assert (is_same<decltype (0.0F + 0.0l), long double>::value);
#if defined(__STDCPP_FLOAT16_T__) && defined(__STDCPP_FLOAT32_T__)
static_assert (!is_same<float16_t, float32_t>::value);
static_assert (is_same<decltype (0.0f16 + 0.0f32), float32_t>::value);
static_assert (is_same<decltype (0.0F32 + 0.0F16), float32_t>::value);
#endif
#if defined(__STDCPP_FLOAT16_T__) && defined(__STDCPP_FLOAT64_T__)
static_assert (!is_same<float16_t, float64_t>::value);
static_assert (is_same<decltype (0.0f16 + 0.0f64), float64_t>::value);
static_assert (is_same<decltype (0.0F64 + 0.0F16), float64_t>::value);
#endif
#if defined(__STDCPP_FLOAT16_T__) && defined(__STDCPP_FLOAT128_T__)
static_assert (!is_same<float16_t, float128_t>::value);
static_assert (is_same<decltype (0.0f16 + 0.0f128), float128_t>::value);
static_assert (is_same<decltype (0.0F128 + 0.0F16), float128_t>::value);
#endif
#if defined(__STDCPP_FLOAT16_T__) && defined(__FLT32X_MANT_DIG__)
static_assert (is_same<decltype (0.0f16 + 0.0f32x), _Float32x>::value);
static_assert (is_same<decltype (0.0F32x + 0.0F16), _Float32x>::value);
#endif
#if defined(__STDCPP_FLOAT16_T__) && defined(__FLT64X_MANT_DIG__)
static_assert (is_same<decltype (0.0f16 + 0.0f64x), _Float64x>::value);
static_assert (is_same<decltype (0.0F64x + 0.0F16), _Float64x>::value);
#endif
#if defined(__STDCPP_FLOAT16_T__) && defined(__FLT128X_MANT_DIG__)
static_assert (is_same<decltype (0.0f16 + 0.0f128x), _Float128x>::value);
static_assert (is_same<decltype (0.0F128x + 0.0F16), _Float128x>::value);
#endif
#if defined(__STDCPP_FLOAT32_T__) && defined(__STDCPP_FLOAT64_T__)
static_assert (!is_same<float32_t, float64_t>::value);
static_assert (is_same<decltype (0.0f32 + 0.0f64), float64_t>::value);
static_assert (is_same<decltype (0.0F64 + 0.0F32), float64_t>::value);
#endif
#if defined(__STDCPP_FLOAT32_T__) && defined(__STDCPP_FLOAT128_T__)
static_assert (!is_same<float32_t, float128_t>::value);
static_assert (is_same<decltype (0.0f32 + 0.0f128), float128_t>::value);
static_assert (is_same<decltype (0.0F128 + 0.0F32), float128_t>::value);
#endif
#if defined(__STDCPP_FLOAT32_T__) && defined(__FLT32X_MANT_DIG__)
static_assert (is_same<decltype (0.0f32 + 0.0f32x), _Float32x>::value);
static_assert (is_same<decltype (0.0F32x + 0.0F32), _Float32x>::value);
#endif
#if defined(__STDCPP_FLOAT32_T__) && defined(__FLT64X_MANT_DIG__)
static_assert (is_same<decltype (0.0f32 + 0.0f64x), _Float64x>::value);
static_assert (is_same<decltype (0.0F64x + 0.0F32), _Float64x>::value);
#endif
#if defined(__STDCPP_FLOAT32_T__) && defined(__FLT128X_MANT_DIG__)
static_assert (is_same<decltype (0.0f32 + 0.0f128x), _Float128x>::value);
static_assert (is_same<decltype (0.0F128x + 0.0F32), _Float128x>::value);
#endif
#if defined(__STDCPP_FLOAT64_T__) && defined(__STDCPP_FLOAT128_T__)
static_assert (!is_same<float64_t, float128_t>::value);
static_assert (is_same<decltype (0.0f64 + 0.0f128), float128_t>::value);
static_assert (is_same<decltype (0.0F128 + 0.0F64), float128_t>::value);
#endif
#if defined(__STDCPP_FLOAT64_T__) && defined(__FLT32X_MANT_DIG__) \
    && __FLT64_MAX_EXP__ == __FLT32X_MAX_EXP__ \
    && __FLT64_MANT_DIG__ == __FLT32X_MANT_DIG__
static_assert (is_same<decltype (0.0f64 + 0.0f32x), float64_t>::value);
static_assert (is_same<decltype (0.0F32x + 0.0F64), float64_t>::value);
#endif
#if defined(__STDCPP_FLOAT64_T__) && defined(__FLT64X_MANT_DIG__)
static_assert (is_same<decltype (0.0f64 + 0.0f64x), _Float64x>::value);
static_assert (is_same<decltype (0.0F64x + 0.0F64), _Float64x>::value);
#endif
#if defined(__STDCPP_FLOAT64_T__) && defined(__FLT128X_MANT_DIG__)
static_assert (is_same<decltype (0.0f64 + 0.0f128x), _Float128x>::value);
static_assert (is_same<decltype (0.0F128x + 0.0F64), _Float128x>::value);
#endif
#if defined(__STDCPP_FLOAT128_T__) && defined(__FLT32X_MANT_DIG__) \
    && __FLT128_MAX_EXP__ >= __FLT32X_MAX_EXP__ \
    && __FLT128_MANT_DIG__ >= __FLT32X_MANT_DIG__
static_assert (is_same<decltype (0.0f128 + 0.0f32x), float128_t>::value);
static_assert (is_same<decltype (0.0F32x + 0.0F128), float128_t>::value);
#endif
#if defined(__STDCPP_FLOAT128_T__) && defined(__FLT64X_MANT_DIG__) \
    && __FLT128_MAX_EXP__ >= __FLT64X_MAX_EXP__ \
    && __FLT128_MANT_DIG__ >= __FLT64X_MANT_DIG__
static_assert (is_same<decltype (0.0f128 + 0.0f64x), float128_t>::value);
static_assert (is_same<decltype (0.0F64x + 0.0F128), float128_t>::value);
#endif
#if defined(__STDCPP_FLOAT128_T__) && defined(__FLT128X_MANT_DIG__)
static_assert (is_same<decltype (0.0f128 + 0.0f128x), _Float128>::value);
static_assert (is_same<decltype (0.0F128x + 0.0F128), _Float128>::value);
#endif
#if defined(__STDCPP_BFLOAT16_T__) && defined(__STDCPP_FLOAT32_T__)
static_assert (!is_same<bfloat16_t, float32_t>::value);
static_assert (is_same<decltype (0.0bf16 + 0.0f32), float32_t>::value);
static_assert (is_same<decltype (0.0F32 + 0.0BF16), float32_t>::value);
#endif
#if defined(__STDCPP_BFLOAT16_T__) && defined(__STDCPP_FLOAT64_T__)
static_assert (!is_same<bfloat16_t, float64_t>::value);
static_assert (is_same<decltype (0.0bf16 + 0.0f64), float64_t>::value);
static_assert (is_same<decltype (0.0F64 + 0.0BF16), float64_t>::value);
#endif
#if defined(__STDCPP_BFLOAT16_T__) && defined(__STDCPP_FLOAT128_T__)
static_assert (!is_same<bfloat16_t, float128_t>::value);
static_assert (is_same<decltype (0.0bf16 + 0.0f128), float128_t>::value);
static_assert (is_same<decltype (0.0F128 + 0.0BF16), float128_t>::value);
#endif
#ifdef __STDCPP_FLOAT16_T__
#if __FLT_MAX_EXP__ > __FLT16_MAX_EXP__ && __FLT_MANT_DIG__ > __FLT16_MANT_DIG__
static_assert (is_same<decltype (0.0f + 0.0f16), float>::value);
static_assert (is_same<decltype (0.0F16 + 0.0F), float>::value);
#endif
#if __DBL_MAX_EXP__ > __FLT16_MAX_EXP__ && __DBL_MANT_DIG__ > __FLT16_MANT_DIG__
static_assert (is_same<decltype (0.0 + 0.0f16), double>::value);
static_assert (is_same<decltype (0.0F16 + 0.0), double>::value);
#endif
#if __LDBL_MAX_EXP__ > __FLT16_MAX_EXP__ && __LDBL_MANT_DIG__ > __FLT16_MANT_DIG__
static_assert (is_same<decltype (0.0L + 0.0f16), long double>::value);
static_assert (is_same<decltype (0.0F16 + 0.0l), long double>::value);
#endif
#endif
#ifdef __STDCPP_FLOAT32_T__
#if __FLT_MAX_EXP__ == __FLT32_MAX_EXP__ && __FLT_MANT_DIG__ == __FLT32_MANT_DIG__
static_assert (is_same<decltype (0.0f + 0.0f32), float32_t>::value);
static_assert (is_same<decltype (0.0F32 + 0.0F), float32_t>::value);
#endif
#if __DBL_MAX_EXP__ > __FLT32_MAX_EXP__ && __DBL_MANT_DIG__ > __FLT32_MANT_DIG__
static_assert (is_same<decltype (0.0 + 0.0f32), double>::value);
static_assert (is_same<decltype (0.0F32 + 0.0), double>::value);
#endif
#if __LDBL_MAX_EXP__ > __FLT32_MAX_EXP__ && __LDBL_MANT_DIG__ > __FLT32_MANT_DIG__
static_assert (is_same<decltype (0.0L + 0.0f32), long double>::value);
static_assert (is_same<decltype (0.0F32 + 0.0l), long double>::value);
#endif
#endif
#ifdef __STDCPP_FLOAT64_T__
#if __FLT_MAX_EXP__ < __FLT64_MAX_EXP__ && __FLT_MANT_DIG__ < __FLT64_MANT_DIG__
static_assert (is_same<decltype (0.0f + 0.0f64), float64_t>::value);
static_assert (is_same<decltype (0.0F64 + 0.0F), float64_t>::value);
#endif
#if __DBL_MAX_EXP__ == __FLT64_MAX_EXP__ && __DBL_MANT_DIG__ == __FLT64_MANT_DIG__
static_assert (is_same<decltype (0.0 + 0.0f64), float64_t>::value);
static_assert (is_same<decltype (0.0F64 + 0.0), float64_t>::value);
#endif
#if __LDBL_MAX_EXP__ > __FLT64_MAX_EXP__ && __LDBL_MANT_DIG__ > __FLT64_MANT_DIG__
static_assert (is_same<decltype (0.0L + 0.0f64), long double>::value);
static_assert (is_same<decltype (0.0F64 + 0.0l), long double>::value);
#endif
#if __LDBL_MAX_EXP__ == __FLT64_MAX_EXP__ && __LDBL_MANT_DIG__ == __FLT64_MANT_DIG__ \
    && __DBL_MAX_EXP__ == __FLT64_MAX_EXP__ && __DBL_MANT_DIG__ == __FLT64_MANT_DIG__
// An extended floating-point type with the same set of values as more than one
// cv-unqualified standard floating-point type has a rank equal to the rank of
// double.
// Then long double will have higher rank than float64_t.
static_assert (is_same<decltype (0.0L + 0.0f64), long double>::value);
static_assert (is_same<decltype (0.0F64 + 0.0l), long double>::value);
#endif
#endif
#ifdef __STDCPP_FLOAT128_T__
#if __FLT_MAX_EXP__ < __FLT128_MAX_EXP__ && __FLT_MANT_DIG__ < __FLT128_MANT_DIG__
static_assert (is_same<decltype (0.0f + 0.0f128), float128_t>::value);
static_assert (is_same<decltype (0.0F128 + 0.0F), float128_t>::value);
#endif
#if __DBL_MAX_EXP__ < __FLT128_MAX_EXP__ && __DBL_MANT_DIG__ < __FLT128_MANT_DIG__
static_assert (is_same<decltype (0.0 + 0.0f128), float128_t>::value);
static_assert (is_same<decltype (0.0F128 + 0.0), float128_t>::value);
#endif
#if __LDBL_MAX_EXP__ <= __FLT128_MAX_EXP__ && __LDBL_MANT_DIG__ <= __FLT128_MANT_DIG__ \
    && __LDBL_MANT_DIG__ != 106 // IBM extended long double and IEEE quad are unordered.
static_assert (is_same<decltype (0.0L + 0.0f128), float128_t>::value);
static_assert (is_same<decltype (0.0F128 + 0.0l), float128_t>::value);
#endif
#ifdef __SIZEOF_FLOAT128__
static_assert (is_same<decltype (0.0Q + 0.0f128), float128_t>::value);
static_assert (is_same<decltype (0.0F128 + 0.0q), float128_t>::value);
#endif
#endif
#ifdef __STDCPP_BFLOAT16_T__
#if __FLT_MAX_EXP__ > __BFLT16_MAX_EXP__ && __FLT_MANT_DIG__ > __BFLT16_MANT_DIG__
static_assert (is_same<decltype (0.0f + 0.0bf16), float>::value);
static_assert (is_same<decltype (0.0BF16 + 0.0F), float>::value);
#endif
#if __DBL_MAX_EXP__ > __BFLT16_MAX_EXP__ && __DBL_MANT_DIG__ > __BFLT16_MANT_DIG__
static_assert (is_same<decltype (0.0 + 0.0bf16), double>::value);
static_assert (is_same<decltype (0.0BF16 + 0.0), double>::value);
#endif
#if __LDBL_MAX_EXP__ > __BFLT16_MAX_EXP__ && __LDBL_MANT_DIG__ > __BFLT16_MANT_DIG__
static_assert (is_same<decltype (0.0L + 0.0bf16), long double>::value);
static_assert (is_same<decltype (0.0BF16 + 0.0l), long double>::value);
#endif
#endif

void foo (float) {}
void foo (double) {}
void foo (long double) {}
#ifdef __STDCPP_FLOAT16_T__
void foo (float16_t) {}
#endif
#ifdef __STDCPP_FLOAT32_T__
void foo (float32_t) {}
#endif
#ifdef __STDCPP_FLOAT64_T__
void foo (float64_t) {}
#endif
#ifdef __STDCPP_FLOAT128_T__
void foo (float128_t) {}
#endif
#ifdef __STDCPP_BFLOAT16_T__
void foo (bfloat16_t) {}
#endif
#ifdef __FLT32X_MANT_DIG__
void foo (_Float32x) {}
#endif
#ifdef __FLT64X_MANT_DIG__
void foo (_Float64x) {}
#endif
#ifdef __FLT128X_MANT_DIG__
void foo (_Float128x) {}
#endif
