// P1467R9 - Extended floating-point types and standard names.
// { dg-do compile { target c++23 } }
// { dg-options "" }

#include "ext-floating.h"

#ifdef __STRICT_ANSI__
#undef __SIZEOF_FLOAT128__
#endif

using namespace std;

float foo (float x, float y, float z) { return x * y + z; }
double foo (double x, double y, double z) { return x * y + z; }
long double foo (long double x, long double y, long double z) { return x * y + z; }
#ifdef __STDCPP_FLOAT16_T__
float16_t foo (float16_t x, float16_t y, float16_t z) { return x * y + z; }
#endif
#ifdef __STDCPP_FLOAT32_T__
float32_t foo (float32_t x, float32_t y, float32_t z) { return x * y + z; }
#endif
#ifdef __STDCPP_FLOAT64_T__
float64_t foo (float64_t x, float64_t y, float64_t z) { return x * y + z; }
#endif
#ifdef __STDCPP_FLOAT128_T__
float128_t foo (float128_t x, float128_t y, float128_t z) { return x * y + z; }
#endif
#ifdef __STDCPP_BFLOAT16_T__
bfloat16_t foo (bfloat16_t x, bfloat16_t y, bfloat16_t z) { return x * y + z; }
#endif
