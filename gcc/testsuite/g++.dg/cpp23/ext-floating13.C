// P1467R9 - Extended floating-point types and standard names.
// { dg-do link { target c++23 } }
// { dg-options "" }

#include <typeinfo>

#ifdef __STDCPP_FLOAT16_T__
const std::type_info &a = typeid(decltype(0.0f16));
#endif
#ifdef __STDCPP_BFLOAT16_T__
const std::type_info &b = typeid(decltype(0.0bf16));
#endif
#ifdef __STDCPP_FLOAT32_T__
const std::type_info &c = typeid(decltype(0.0f32));
#endif
#ifdef __STDCPP_FLOAT64_T__
const std::type_info &d = typeid(decltype(0.0f64));
#endif
#ifdef __STDCPP_FLOAT128_T__
const std::type_info &e = typeid(decltype(0.0f128));
#endif
#ifdef __FLT32X_MAX__
const std::type_info &f = typeid(decltype(0.0f32x));
#endif
#ifdef __FLT64X_MAX__
const std::type_info &g = typeid(decltype(0.0f64x));
#endif
#ifdef __FLT128X_MAX__
const std::type_info &h = typeid(decltype(0.0f128x));
#endif

int
main ()
{
}
