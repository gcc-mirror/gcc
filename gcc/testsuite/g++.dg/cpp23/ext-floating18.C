// P1467R9 - Extended floating-point types and standard names.
// { dg-do compile { target c++23 } }
// { dg-options "" }
// { dg-add-options float32 }

constexpr int foo (float) { return 1; }
constexpr int foo (double) { return 2; }
constexpr int foo (long double) { return 3; }

#ifdef __STDCPP_FLOAT32_T__
#if __FLT_MAX_EXP__ == __FLT32_MAX_EXP__ \
    && __FLT_MAX_DIG__ == __FLT32_MAX_DIG__
#if __FLT_MAX_EXP__ == __DBL_MAX_EXP__ \
    && __FLT_MAX_DIG__ == __DBL_MAX_DIG__
static_assert (foo (1.0f32) == 2);
#else
static_assert (foo (1.0f32) == 1);
#endif
#endif
#endif
#ifdef __STDCPP_FLOAT64_T__
#if __DBL_MAX_EXP__ == __FLT64_MAX_EXP__ \
    && __DBL_MAX_DIG__ == __FLT64_MAX_DIG__
static_assert (foo (1.0f64) == 2);
#endif
#endif
