// DR 2836 - Conversion rank of long double and extended floating-point types
// { dg-do compile { target c++23 } }
// { dg-additional-options "-mlong-double-64" { target powerpc*-*-* s390*-*-* } }

#include <stdfloat>

#if defined (__STDCPP_FLOAT64_T__) \
    && __LDBL_MAX_EXP__ == __FLT64_MAX_EXP__ \
    && __LDBL_MANT_DIG__ == __FLT64_MANT_DIG__ \
    && __DBL_MAX_EXP__ == __FLT64_MAX_EXP__ \
    && __DBL_MANT_DIG__ == __FLT64_MANT_DIG__
auto
foo (long double x, std::float64_t y)
{
  return x + y;
}

template<typename T, T v> struct integral_constant {
  static constexpr T value = v;
};
typedef integral_constant<bool, false> false_type;
typedef integral_constant<bool, true> true_type;
template<class T, class U>
struct is_same : false_type {};
template <class T>
struct is_same<T, T> : true_type {};

static_assert (is_same <decltype (foo (1.0L, 1.0F64)), long double>::value);

#endif
