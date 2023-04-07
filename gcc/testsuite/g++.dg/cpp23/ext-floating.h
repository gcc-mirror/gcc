// P1467R9 - Extended floating-point types and standard names.

namespace std
{
  #ifdef __STDCPP_FLOAT16_T__
  using float16_t = _Float16;
  #endif
  #ifdef __STDCPP_FLOAT32_T__
  using float32_t = _Float32;
  #endif
  #ifdef __STDCPP_FLOAT64_T__
  using float64_t = _Float64;
  #endif
  #ifdef __STDCPP_FLOAT128_T__
  using float128_t = _Float128;
  #endif
  #ifdef __STDCPP_BFLOAT16_T__
  using bfloat16_t = decltype (0.0bf16);
  #endif
  template<typename T, T v> struct integral_constant {
    static constexpr T value = v;
  };
  typedef integral_constant<bool, false> false_type;
  typedef integral_constant<bool, true> true_type;
  template<class T, class U>
  struct is_same : std::false_type {};
  template <class T>
  struct is_same<T, T> : std::true_type {};
}
