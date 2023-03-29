// P1467R9 - Extended floating-point types and standard names.
// Variant of ext-floating3.C test with different specific assumptions
// about float, double, long double.
// float, double and long double are assumed to be IEEE 754 single, double
// and quad.
// { dg-do compile { target { c++23 && { aarch64*-*-* powerpc64le*-*-linux* riscv*-*-* s390*-*-* sparc*-*-linux* } } } }
// { dg-options "" }
// { dg-additional-options "-mlong-double-128" { target s390*-*-* sparc*-*-linux* } }
// { dg-additional-options "-mvsx -mfloat128 -mlong-double-128 -mabi=ieeelongdouble -Wno-psabi" { target powerpc64le*-*-linux* } }

#include "ext-floating.h"

#if !defined(__STDCPP_FLOAT32_T__) \
    || !defined(__STDCPP_FLOAT64_T__) || !defined(__STDCPP_FLOAT128_T__) \
    || __FLT_MAX_EXP__ != __FLT32_MAX_EXP__ || __FLT_MANT_DIG__ != __FLT32_MANT_DIG__ \
    || __DBL_MAX_EXP__ != __FLT64_MAX_EXP__ || __DBL_MANT_DIG__ != __FLT64_MANT_DIG__ \
    || __LDBL_MAX_EXP__ != __FLT128_MAX_EXP__ || __LDBL_MANT_DIG__ != __FLT128_MANT_DIG__
#error Unexpected set of floating point types
#endif

using namespace std;

#ifdef __STDCPP_FLOAT16_T__
float16_t f16i = 1.0f;			// { dg-warning "converting to 'std::float16_t' \\\{aka '_Float16'\\\} from 'float' with greater conversion rank" "" { target float16 } }
float16_t f16k = 1.0;			// { dg-warning "converting to 'std::float16_t' \\\{aka '_Float16'\\\} from 'double' with greater conversion rank" "" { target float16 } }
float16_t f16m = 1.0L;			// { dg-warning "converting to 'std::float16_t' \\\{aka '_Float16'\\\} from 'long double' with greater conversion rank" "" { target float16 } }
#endif
float32_t f32i = 1.0f;
float32_t f32k = 1.0;			// { dg-warning "converting to 'std::float32_t' \\\{aka '_Float32'\\\} from 'double' with greater conversion rank" }
float32_t f32m = 1.0L;			// { dg-warning "converting to 'std::float32_t' \\\{aka '_Float32'\\\} from 'long double' with greater conversion rank" }
float64_t f64i = 1.0f;
float64_t f64k = 1.0;
float64_t f64m = 1.0L;			// { dg-warning "converting to 'std::float64_t' \\\{aka '_Float64'\\\} from 'long double' with greater conversion rank" }
float128_t f128i = 1.0f;
float128_t f128k = 1.0;
float128_t f128m = 1.0L;

#ifdef __STDCPP_FLOAT16_T__
constexpr float16_t f16x = 1.0F16;
#endif
constexpr float32_t f32x = 2.0F32;
constexpr float64_t f64x = 3.0F64;
constexpr float128_t f128x = 4.0F128;
constexpr float fx = 5.0f;
constexpr double dx = 6.0;
constexpr long double ldx = 7.0L;

constexpr int foo (float32_t) { return 1; }
constexpr int foo (float64_t) { return 2; }
constexpr int bar (float) { return 3; }
constexpr int bar (double) { return 4; }
constexpr int bar (long double) { return 5; }
constexpr int baz (float32_t) { return 6; }
constexpr int baz (float64_t) { return 7; }
constexpr int baz (float128_t) { return 8; }
constexpr int qux (float64_t) { return 9; }
constexpr int qux (float32_t) { return 10; }
constexpr int fred (long double) { return 11; }
constexpr int fred (double) { return 12; }
constexpr int fred (float) { return 13; }
constexpr int thud (float128_t) { return 14; }
constexpr int thud (float64_t) { return 15; }
constexpr int thud (float32_t) { return 16; }
struct S {
  constexpr operator float32_t () const { return 1.0f32; }
  constexpr operator float64_t () const { return 2.0f64; }
};
struct T {
  constexpr operator float64_t () const { return 3.0f64; }
  constexpr operator float32_t () const { return 4.0f32; }
};

void
test (S s, T t)
{
#ifdef __STDCPP_FLOAT16_T__
  foo (float16_t (1.0));			// { dg-error "call of overloaded 'foo\\\(std::float16_t\\\)' is ambiguous" "" { target float16 } }
#endif
  static_assert (foo (float (2.0)) == 1);
  static_assert (foo (double (3.0)) == 2);
  constexpr double x (s);
  static_assert (x == 2.0);
#ifdef __STDCPP_FLOAT16_T__
  bar (f16x);					// { dg-error "call of overloaded 'bar\\\(const std::float16_t\\\&\\\)' is ambiguous" "" { target float16 } }
#endif
  static_assert (bar (f32x) == 3);
  static_assert (bar (f64x) == 4);
  static_assert (bar (f128x) == 5);
  static_assert (bar (fx) == 3);
  static_assert (bar (dx) == 4);
  static_assert (bar (ldx) == 5);
#ifdef __STDCPP_FLOAT16_T__
  baz (f16x);					// { dg-error "call of overloaded 'baz\\\(const std::float16_t\\\&\\\)' is ambiguous" "" { target float16 } }
#endif
  static_assert (baz (f32x) == 6);
  static_assert (baz (f64x) == 7);
  static_assert (baz (f128x) == 8);
  static_assert (baz (fx) == 6);
  static_assert (baz (dx) == 7);
  static_assert (baz (ldx) == 8);
#ifdef __STDCPP_FLOAT16_T__
  qux (float16_t (1.0));			// { dg-error "call of overloaded 'qux\\\(std::float16_t\\\)' is ambiguous" "" { target float16 } }
#endif
  static_assert (qux (float (2.0)) == 10);
  static_assert (qux (double (3.0)) == 9);
  constexpr double y (t);
  static_assert (y == 3.0);
#ifdef __STDCPP_FLOAT16_T__
  fred (f16x);					// { dg-error "call of overloaded 'fred\\\(const std::float16_t\\\&\\\)' is ambiguous" "" { target float16 } }
#endif
  static_assert (fred (f32x) == 13);
  static_assert (fred (f64x) == 12);
  static_assert (fred (f128x) == 11);
  static_assert (fred (fx) == 13);
  static_assert (fred (dx) == 12);
  static_assert (fred (ldx) == 11);
#ifdef __STDCPP_FLOAT16_T__
  thud (f16x);					// { dg-error "call of overloaded 'thud\\\(const std::float16_t\\\&\\\)' is ambiguous" "" { target float16 } }
#endif
  static_assert (thud (f32x) == 16);
  static_assert (thud (f64x) == 15);
  static_assert (thud (f128x) == 14);
  static_assert (thud (fx) == 16);
  static_assert (thud (dx) == 15);
  static_assert (thud (ldx) == 14);
}
