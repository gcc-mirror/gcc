// P1467R9 - Extended floating-point types and standard names.
// { dg-do compile { target { c++23 && { i?86-*-linux* x86_64-*-linux* } } } }
// { dg-options "" }
// { dg-add-options float16 }
// { dg-add-options bfloat16 }
// { dg-skip-if "requires hosted libstdc++ for complex" { ! hostedlib } }

#include <complex>
#include <stdfloat>

#if !defined(__STDCPP_FLOAT32_T__) \
    || !defined(__STDCPP_FLOAT64_T__) || !defined(__STDCPP_FLOAT128_T__) \
    || __FLT_MAX_EXP__ != __FLT32_MAX_EXP__ || __FLT_MANT_DIG__ != __FLT32_MANT_DIG__ \
    || __DBL_MAX_EXP__ != __FLT64_MAX_EXP__ || __DBL_MANT_DIG__ != __FLT64_MANT_DIG__ \
    || __LDBL_MAX_EXP__ != __FLT128_MAX_EXP__ || __LDBL_MANT_DIG__ >= __FLT128_MANT_DIG__ \
    || !defined(__SIZEOF_FLOAT128__)
#error Unexpected set of floating point types
#endif

using namespace std;

int
main()
{
  complex<float> a01(complex<float>(1.0f, 2.0f));
  complex<float> a02 = complex<float>(1.0f, 2.0f);
  complex<float> a03(complex<double>(1.0, 2.0));
  complex<float> a04 = complex<double>(1.0, 2.0);			// { dg-error "conversion from 'complex<double>' to non-scalar type 'complex<float>' requested" }
  complex<float> a05(complex<long double>(1.0L, 2.0L));
  complex<float> a06 = complex<long double>(1.0L, 2.0L);		// { dg-error "conversion from 'complex<long double>' to non-scalar type 'complex<float>' requested" }
  complex<float> a07(complex<float32_t>(1.0f32, 2.0f32));
  complex<float> a08 = complex<float32_t>(1.0f32, 2.0f32);
  complex<float> a09(complex<float64_t>(1.0f64, 2.0f64));
  complex<float> a10 = complex<float64_t>(1.0f64, 2.0f64);		// { dg-error "conversion from 'complex<_Float64>' to non-scalar type 'complex<float>' requested" }
  complex<float> a11(complex<float128_t>(1.0f128, 2.0f128));
  complex<float> a12 = complex<float128_t>(1.0f128, 2.0f128);		// { dg-error "conversion from 'complex<_Float128>' to non-scalar type 'complex<float>' requested" }
#ifdef __STDCPP_FLOAT16_T__
  complex<float> a13(complex<float16_t>(1.0f16, 2.0f16));
  complex<float> a14 = complex<float16_t>(1.0f16, 2.0f16);
#endif
#ifdef __STDCPP_BFLOAT16_T__
  complex<float> a15(complex<bfloat16_t>(1.0bf16, 2.0bf16));
  complex<float> a16 = complex<bfloat16_t>(1.0bf16, 2.0bf16);
#endif
  complex<double> b01(complex<float>(1.0f, 2.0f));
  complex<double> b02 = complex<float>(1.0f, 2.0f);
  complex<double> b03(complex<double>(1.0, 2.0));
  complex<double> b04 = complex<double>(1.0, 2.0);
  complex<double> b05(complex<long double>(1.0L, 2.0L));
  complex<double> b06 = complex<long double>(1.0L, 2.0L);		// { dg-error "conversion from 'complex<long double>' to non-scalar type 'complex<double>' requested" }
  complex<double> b07(complex<float32_t>(1.0f32, 2.0f32));
  complex<double> b08 = complex<float32_t>(1.0f32, 2.0f32);
  complex<double> b09(complex<float64_t>(1.0f64, 2.0f64));
  complex<double> b10 = complex<float64_t>(1.0f64, 2.0f64);
  complex<double> b11(complex<float128_t>(1.0f128, 2.0f128));
  complex<double> b12 = complex<float128_t>(1.0f128, 2.0f128);		// { dg-error "conversion from 'complex<_Float128>' to non-scalar type 'complex<double>' requested" }
#ifdef __STDCPP_FLOAT16_T__
  complex<double> b13(complex<float16_t>(1.0f16, 2.0f16));
  complex<double> b14 = complex<float16_t>(1.0f16, 2.0f16);
#endif
#ifdef __STDCPP_BFLOAT16_T__
  complex<double> b15(complex<bfloat16_t>(1.0bf16, 2.0bf16));
  complex<double> b16 = complex<bfloat16_t>(1.0bf16, 2.0bf16);
#endif
  complex<long double> c01(complex<float>(1.0f, 2.0f));
  complex<long double> c02 = complex<float>(1.0f, 2.0f);
  complex<long double> c03(complex<double>(1.0, 2.0));
  complex<long double> c04 = complex<double>(1.0, 2.0);
  complex<long double> c05(complex<long double>(1.0L, 2.0L));
  complex<long double> c06 = complex<long double>(1.0L, 2.0L);
  complex<long double> c07(complex<float32_t>(1.0f32, 2.0f32));
  complex<long double> c08 = complex<float32_t>(1.0f32, 2.0f32);
  complex<long double> c09(complex<float64_t>(1.0f64, 2.0f64));
  complex<long double> c10 = complex<float64_t>(1.0f64, 2.0f64);
  complex<long double> c11(complex<float128_t>(1.0f128, 2.0f128));
  complex<long double> c12 = complex<float128_t>(1.0f128, 2.0f128);	// { dg-error "conversion from 'complex<_Float128>' to non-scalar type 'complex<long double>' requested" }
#ifdef __STDCPP_FLOAT16_T__
  complex<long double> c13(complex<float16_t>(1.0f16, 2.0f16));
  complex<long double> c14 = complex<float16_t>(1.0f16, 2.0f16);
#endif
#ifdef __STDCPP_BFLOAT16_T__
  complex<long double> c15(complex<bfloat16_t>(1.0bf16, 2.0bf16));
  complex<long double> c16 = complex<bfloat16_t>(1.0bf16, 2.0bf16);
#endif
  complex<float32_t> d01(complex<float>(1.0f, 2.0f));
  complex<float32_t> d02 = complex<float>(1.0f, 2.0f);
  complex<float32_t> d03(complex<double>(1.0, 2.0));
  complex<float32_t> d04 = complex<double>(1.0, 2.0);			// { dg-error "conversion from 'complex<double>' to non-scalar type 'complex<_Float32>' requested" }
  complex<float32_t> d05(complex<long double>(1.0L, 2.0L));
  complex<float32_t> d06 = complex<long double>(1.0L, 2.0L);		// { dg-error "conversion from 'complex<long double>' to non-scalar type 'complex<_Float32>' requested" }
  complex<float32_t> d07(complex<float32_t>(1.0f32, 2.0f32));
  complex<float32_t> d08 = complex<float32_t>(1.0f32, 2.0f32);
  complex<float32_t> d09(complex<float64_t>(1.0f64, 2.0f64));
  complex<float32_t> d10 = complex<float64_t>(1.0f64, 2.0f64);		// { dg-error "conversion from 'complex<_Float64>' to non-scalar type 'complex<_Float32>' requested" }
  complex<float32_t> d11(complex<float128_t>(1.0f128, 2.0f128));
  complex<float32_t> d12 = complex<float128_t>(1.0f128, 2.0f128);	// { dg-error "conversion from 'complex<_Float128>' to non-scalar type 'complex<_Float32>' requested" }
#ifdef __STDCPP_FLOAT16_T__
  complex<float32_t> d13(complex<float16_t>(1.0f16, 2.0f16));
  complex<float32_t> d14 = complex<float16_t>(1.0f16, 2.0f16);
#endif
#ifdef __STDCPP_BFLOAT16_T__
  complex<float32_t> d15(complex<bfloat16_t>(1.0bf16, 2.0bf16));
  complex<float32_t> d16 = complex<bfloat16_t>(1.0bf16, 2.0bf16);
#endif
  complex<float64_t> e01(complex<float>(1.0f, 2.0f));
  complex<float64_t> e02 = complex<float>(1.0f, 2.0f);
  complex<float64_t> e03(complex<double>(1.0, 2.0));
  complex<float64_t> e04 = complex<double>(1.0, 2.0);
  complex<float64_t> e05(complex<long double>(1.0L, 2.0L));
  complex<float64_t> e06 = complex<long double>(1.0L, 2.0L);		// { dg-error "conversion from 'complex<long double>' to non-scalar type 'complex<_Float64>' requested" }
  complex<float64_t> e07(complex<float32_t>(1.0f32, 2.0f32));
  complex<float64_t> e08 = complex<float32_t>(1.0f32, 2.0f32);
  complex<float64_t> e09(complex<float64_t>(1.0f64, 2.0f64));
  complex<float64_t> e10 = complex<float64_t>(1.0f64, 2.0f64);
  complex<float64_t> e11(complex<float128_t>(1.0f128, 2.0f128));
  complex<float64_t> e12 = complex<float128_t>(1.0f128, 2.0f128);	// { dg-error "conversion from 'complex<_Float128>' to non-scalar type 'complex<_Float64>' requested" }
#ifdef __STDCPP_FLOAT16_T__
  complex<float64_t> e13(complex<float16_t>(1.0f16, 2.0f16));
  complex<float64_t> e14 = complex<float16_t>(1.0f16, 2.0f16);
#endif
#ifdef __STDCPP_BFLOAT16_T__
  complex<float64_t> e15(complex<bfloat16_t>(1.0bf16, 2.0bf16));
  complex<float64_t> e16 = complex<bfloat16_t>(1.0bf16, 2.0bf16);
#endif
  complex<float128_t> f01(complex<float>(1.0f, 2.0f));
  complex<float128_t> f02 = complex<float>(1.0f, 2.0f);
  complex<float128_t> f03(complex<double>(1.0, 2.0));
  complex<float128_t> f04 = complex<double>(1.0, 2.0);
  complex<float128_t> f05(complex<long double>(1.0L, 2.0L));
  complex<float128_t> f06 = complex<long double>(1.0L, 2.0L);
  complex<float128_t> f07(complex<float32_t>(1.0f32, 2.0f32));
  complex<float128_t> f08 = complex<float32_t>(1.0f32, 2.0f32);
  complex<float128_t> f09(complex<float64_t>(1.0f64, 2.0f64));
  complex<float128_t> f10 = complex<float64_t>(1.0f64, 2.0f64);
  complex<float128_t> f11(complex<float128_t>(1.0f128, 2.0f128));
  complex<float128_t> f12 = complex<float128_t>(1.0f128, 2.0f128);
#ifdef __STDCPP_FLOAT16_T__
  complex<float128_t> f13(complex<float16_t>(1.0f16, 2.0f16));
  complex<float128_t> f14 = complex<float16_t>(1.0f16, 2.0f16);
#endif
#ifdef __STDCPP_BFLOAT16_T__
  complex<float128_t> f15(complex<bfloat16_t>(1.0bf16, 2.0bf16));
  complex<float128_t> f16 = complex<bfloat16_t>(1.0bf16, 2.0bf16);
#endif
#ifdef __STDCPP_FLOAT16_T__
  complex<float16_t> g01(complex<float>(1.0f, 2.0f));
  complex<float16_t> g02 = complex<float>(1.0f, 2.0f);			// { dg-error "conversion from 'complex<float>' to non-scalar type 'complex<_Float16>' requested" "" { target float16 } }
  complex<float16_t> g03(complex<double>(1.0, 2.0));
  complex<float16_t> g04 = complex<double>(1.0, 2.0);			// { dg-error "conversion from 'complex<double>' to non-scalar type 'complex<_Float16>' requested" "" { target float16 } }
  complex<float16_t> g05(complex<long double>(1.0L, 2.0L));
  complex<float16_t> g06 = complex<long double>(1.0L, 2.0L);		// { dg-error "conversion from 'complex<long double>' to non-scalar type 'complex<_Float16>' requested" "" { target float16 } }
  complex<float16_t> g07(complex<float32_t>(1.0f32, 2.0f32));
  complex<float16_t> g08 = complex<float32_t>(1.0f32, 2.0f32);		// { dg-error "conversion from 'complex<_Float32>' to non-scalar type 'complex<_Float16>' requested" "" { target float16 } }
  complex<float16_t> g09(complex<float64_t>(1.0f64, 2.0f64));
  complex<float16_t> g10 = complex<float64_t>(1.0f64, 2.0f64);		// { dg-error "conversion from 'complex<_Float64>' to non-scalar type 'complex<_Float16>' requested" "" { target float16 } }
  complex<float16_t> g11(complex<float128_t>(1.0f128, 2.0f128));
  complex<float16_t> g12 = complex<float128_t>(1.0f128, 2.0f128);	// { dg-error "conversion from 'complex<_Float128>' to non-scalar type 'complex<_Float16>' requested" "" { target float16 } }
  complex<float16_t> g13(complex<float16_t>(1.0f16, 2.0f16));
  complex<float16_t> g14 = complex<float16_t>(1.0f16, 2.0f16);
#ifdef __STDCPP_BFLOAT16_T__
  complex<float16_t> g15(complex<bfloat16_t>(1.0bf16, 2.0bf16));
  complex<float16_t> g16 = complex<bfloat16_t>(1.0bf16, 2.0bf16);	// { dg-error "conversion from 'complex<\[^\n\r]*>' to non-scalar type 'complex<_Float16>' requested" "" { target { float16 && bfloat16 } } }
#endif
#endif
#ifdef __STDCPP_BFLOAT16_T__
  complex<bfloat16_t> h01(complex<float>(1.0f, 2.0f));
  complex<bfloat16_t> h02 = complex<float>(1.0f, 2.0f);			// { dg-error "conversion from 'complex<float>' to non-scalar type 'complex<\[^\n\r]*>' requested" "" { target bfloat16 } }
  complex<bfloat16_t> h03(complex<double>(1.0, 2.0));
  complex<bfloat16_t> h04 = complex<double>(1.0, 2.0);			// { dg-error "conversion from 'complex<double>' to non-scalar type 'complex<\[^\n\r]*>' requested" "" { target bfloat16 } }
  complex<bfloat16_t> h05(complex<long double>(1.0L, 2.0L));
  complex<bfloat16_t> h06 = complex<long double>(1.0L, 2.0L);		// { dg-error "conversion from 'complex<long double>' to non-scalar type 'complex<\[^\n\r]*>' requested" "" { target bfloat16 } }
  complex<bfloat16_t> h07(complex<float32_t>(1.0f32, 2.0f32));
  complex<bfloat16_t> h08 = complex<float32_t>(1.0f32, 2.0f32);		// { dg-error "conversion from 'complex<_Float32>' to non-scalar type 'complex<\[^\n\r]*>' requested" "" { target bfloat16 } }
  complex<bfloat16_t> h09(complex<float64_t>(1.0f64, 2.0f64));
  complex<bfloat16_t> h10 = complex<float64_t>(1.0f64, 2.0f64);		// { dg-error "conversion from 'complex<_Float64>' to non-scalar type 'complex<\[^\n\r]*>' requested" "" { target bfloat16 } }
  complex<bfloat16_t> h11(complex<float128_t>(1.0f128, 2.0f128));
  complex<bfloat16_t> h12 = complex<float128_t>(1.0f128, 2.0f128);	// { dg-error "conversion from 'complex<_Float128>' to non-scalar type 'complex<\[^\n\r]*>' requested" "" { target bfloat16 } }
#ifdef __STDCPP_FLOAT16_T__
  complex<bfloat16_t> h13(complex<float16_t>(1.0f16, 2.0f16));
  complex<bfloat16_t> h14 = complex<float16_t>(1.0f16, 2.0f16);		// { dg-error "conversion from 'complex<_Float16>' to non-scalar type 'complex<\[^\n\r]*>' requested" "" { target { float16 && bfloat16 } } }
#endif
  complex<bfloat16_t> h15(complex<bfloat16_t>(1.0bf16, 2.0bf16));
  complex<bfloat16_t> h16 = complex<bfloat16_t>(1.0bf16, 2.0bf16);
#endif
}
