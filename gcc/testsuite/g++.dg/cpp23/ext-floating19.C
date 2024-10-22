// { dg-do compile { target c++11 } }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++23-extensions"

#ifdef __STDCPP_FLOAT16_T__
auto x16 = 3.14f16;
#endif
#ifdef __STDCPP_FLOAT32_T__
auto x32 = 3.14f32;
#endif
#ifdef __STDCPP_FLOAT64_T__
auto x64 = 3.14f64;
#endif
#ifdef __STDCPP_FLOAT128_T__
auto x128 = 3.14f128;
#endif
#ifdef __STDCPP_BFLOAT16_T__
auto xbf = 1.2bf16;
#endif
