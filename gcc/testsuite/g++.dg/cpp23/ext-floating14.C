// P1467R9 - Extended floating-point types and standard names.
// PR target/107703
// { dg-do run { target c++23 } }
// { dg-options "-fexcess-precision=standard" }

#include "ext-floating.h"

#ifdef __SIZEOF_INT128__
#define INT128_MAX ((signed __int128) ((~(unsigned __int128) 0) >> 1))
#endif

template <typename T, typename F>
[[gnu::noipa]] T cvt (F f)
{
  return T (F (f));
}

int
main ()
{
  // __FLT32_MAX_EXP__ is 128, so make sure all unsigned long long and unsigned __int128
  // values fit into it.  __FLT16_MAX__ is 65504.0f16, so we need to be
  // careful for that.
#if __SIZEOF_LONG_LONG__ * __CHAR_BIT__ <= 128
#if !defined(__SIZEOF_INT128__) || __SIZEOF_INT128__ * __CHAR_BIT__ == 128
#ifdef __STDCPP_FLOAT16_T__
  if (cvt <std::float16_t, signed char> (42) != (std::float16_t) 42
      || cvt <std::float16_t, signed char> (-42) != (std::float16_t) -42
#if __SCHAR_MAX__ < 65504
      || cvt <std::float16_t, signed char> (__SCHAR_MAX__) != (std::float16_t) __SCHAR_MAX__
      || cvt <std::float16_t, signed char> (-__SCHAR_MAX__ - 1) != (std::float16_t) (-__SCHAR_MAX__ - 1)
#endif
     )
    __builtin_abort ();
  if (cvt <std::float16_t, unsigned char> (42) != (std::float16_t) 42
#if __SCHAR_MAX__ * 2 + 1 < 65504
      || cvt <std::float16_t, unsigned char> ((unsigned char) ~0) != (std::float16_t) ((unsigned char) ~0)
#endif
     )
    __builtin_abort ();
  if (cvt <std::float16_t, signed short> (42) != (std::float16_t) 42
      || cvt <std::float16_t, signed short> (-42) != (std::float16_t) -42
#if __SHRT_MAX__ < 65504
      || cvt <std::float16_t, signed short> (__SHRT_MAX__) != (std::float16_t) __SHRT_MAX__
      || cvt <std::float16_t, signed short> (-__SHRT_MAX__ - 1) != (std::float16_t) (-__SHRT_MAX__ - 1)
#else
      || cvt <std::float16_t, signed short> (65504) != (std::float16_t) 65504
      || cvt <std::float16_t, signed short> (-65504) != (std::float16_t) -65504
#endif
     )
    __builtin_abort ();
  if (cvt <std::float16_t, unsigned short> (42) != (std::float16_t) 42
      || cvt <std::float16_t, unsigned short> (65504U) != (std::float16_t) 65504U)
    __builtin_abort ();
  if (cvt <std::float16_t, signed int> (42) != (std::float16_t) 42
      || cvt <std::float16_t, signed int> (-42) != (std::float16_t) -42
#if __INT_MAX__ < 65504
      || cvt <std::float16_t, signed short> (__INT_MAX__) != (std::float16_t) __INT_MAX__
      || cvt <std::float16_t, signed short> (-__INT_MAX__ - 1) != (std::float16_t) (-__INT_MAX__ - 1)
#else
      || cvt <std::float16_t, signed int> (65504) != (std::float16_t) 65504
      || cvt <std::float16_t, signed int> (-65504) != (std::float16_t) -65504
#endif
     )
    __builtin_abort ();
  if (cvt <std::float16_t, unsigned int> (42) != (std::float16_t) 42U
      || cvt <std::float16_t, unsigned int> (65504U) != (std::float16_t) 65504U)
    __builtin_abort ();
  if (cvt <std::float16_t, signed long int> (42L) != (std::float16_t) 42L
      || cvt <std::float16_t, signed long int> (-42L) != (std::float16_t) -42L
      || cvt <std::float16_t, signed long int> (65504L) != (std::float16_t) 65504L
      || cvt <std::float16_t, signed long int> (-65504L) != (std::float16_t) -65504L)
    __builtin_abort ();
  if (cvt <std::float16_t, unsigned long int> (42UL) != (std::float16_t) 42UL
      || cvt <std::float16_t, unsigned long int> (65504UL) != (std::float16_t) 65504UL)
    __builtin_abort ();
  if (cvt <std::float16_t, signed long long int> (42LL) != (std::float16_t) 42LL
      || cvt <std::float16_t, signed long long int> (-42LL) != (std::float16_t) -42LL
      || cvt <std::float16_t, signed long long int> (65504LL) != (std::float16_t) 65504LL
      || cvt <std::float16_t, signed long long int> (-65504LL) != (std::float16_t) -65504LL)
    __builtin_abort ();
  if (cvt <std::float16_t, unsigned long long int> (42ULL) != (std::float16_t) 42ULL
      || cvt <std::float16_t, unsigned long long int> (65504ULL) != (std::float16_t) 65504ULL)
    __builtin_abort ();
#ifdef __SIZEOF_INT128__
  if (cvt <std::float16_t, signed __int128> (42LL) != (std::float16_t) (signed __int128) 42LL
      || cvt <std::float16_t, signed __int128> (-42LL) != (std::float16_t) (signed __int128) -42LL
      || cvt <std::float16_t, signed __int128> (65504LL) != (std::float16_t) (signed __int128) 65504LL
      || cvt <std::float16_t, signed __int128> (-65504LL) != (std::float16_t) (signed __int128) -65504LL)
    __builtin_abort ();
  if (cvt <std::float16_t, unsigned __int128> (42ULL) != (std::float16_t) (unsigned __int128) 42ULL
      || cvt <std::float16_t, unsigned __int128> (65504ULL) != (std::float16_t) (unsigned __int128) 65504ULL)
    __builtin_abort ();
#endif
#endif
#ifdef __STDCPP_BFLOAT16_T__
  if (cvt <std::bfloat16_t, signed char> (42) != (std::bfloat16_t) 42
      || cvt <std::bfloat16_t, signed char> (-42) != (std::bfloat16_t) -42
      || cvt <std::bfloat16_t, signed char> (__SCHAR_MAX__) != (std::bfloat16_t) __SCHAR_MAX__
      || cvt <std::bfloat16_t, signed char> (-__SCHAR_MAX__ - 1) != (std::bfloat16_t) (-__SCHAR_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::bfloat16_t, unsigned char> (42) != (std::bfloat16_t) 42
      || cvt <std::bfloat16_t, unsigned char> ((unsigned char) ~0) != (std::bfloat16_t) ((unsigned char) ~0))
    __builtin_abort ();
  if (cvt <std::bfloat16_t, signed short> (42) != (std::bfloat16_t) 42
      || cvt <std::bfloat16_t, signed short> (-42) != (std::bfloat16_t) -42
      || cvt <std::bfloat16_t, signed short> (__SHRT_MAX__) != (std::bfloat16_t) __SHRT_MAX__
      || cvt <std::bfloat16_t, signed short> (-__SHRT_MAX__ - 1) != (std::bfloat16_t) (-__SHRT_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::bfloat16_t, unsigned short> (42) != (std::bfloat16_t) 42
      || cvt <std::bfloat16_t, unsigned short> ((unsigned short) ~0) != (std::bfloat16_t) ((unsigned short) ~0))
    __builtin_abort ();
  if (cvt <std::bfloat16_t, signed int> (42) != (std::bfloat16_t) 42
      || cvt <std::bfloat16_t, signed int> (-42) != (std::bfloat16_t) -42
      || cvt <std::bfloat16_t, signed int> (__INT_MAX__) != (std::bfloat16_t) __INT_MAX__
      || cvt <std::bfloat16_t, signed int> (-__INT_MAX__ - 1) != (std::bfloat16_t) (-__INT_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::bfloat16_t, unsigned int> (42) != (std::bfloat16_t) 42U
      || cvt <std::bfloat16_t, unsigned int> (~0U) != (std::bfloat16_t) ~0U)
    __builtin_abort ();
  if (cvt <std::bfloat16_t, signed long int> (42L) != (std::bfloat16_t) 42L
      || cvt <std::bfloat16_t, signed long int> (-42L) != (std::bfloat16_t) -42L
      || cvt <std::bfloat16_t, signed long int> (__LONG_MAX__) != (std::bfloat16_t) __LONG_MAX__
      || cvt <std::bfloat16_t, signed long int> (-__LONG_MAX__ - 1) != (std::bfloat16_t) (-__LONG_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::bfloat16_t, unsigned long int> (42UL) != (std::bfloat16_t) 42UL
      || cvt <std::bfloat16_t, unsigned long int> (~0UL) != (std::bfloat16_t) ~0UL)
    __builtin_abort ();
  if (cvt <std::bfloat16_t, signed long long int> (42LL) != (std::bfloat16_t) 42LL
      || cvt <std::bfloat16_t, signed long long int> (-42LL) != (std::bfloat16_t) -42LL
      || cvt <std::bfloat16_t, signed long long int> (__LONG_LONG_MAX__) != (std::bfloat16_t) __LONG_LONG_MAX__
      || cvt <std::bfloat16_t, signed long long int> (-__LONG_LONG_MAX__ - 1) != (std::bfloat16_t) (-__LONG_LONG_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::bfloat16_t, unsigned long long int> (42ULL) != (std::bfloat16_t) 42ULL
      || cvt <std::bfloat16_t, unsigned long long int> (~0ULL) != (std::bfloat16_t) ~0ULL)
    __builtin_abort ();
#ifdef __SIZEOF_INT128__
  if (cvt <std::bfloat16_t, signed __int128> (42LL) != (std::bfloat16_t) (signed __int128) 42LL
      || cvt <std::bfloat16_t, signed __int128> (-42LL) != (std::bfloat16_t) (signed __int128) -42LL
      || cvt <std::bfloat16_t, signed __int128> (INT128_MAX) != (std::bfloat16_t) INT128_MAX
      || cvt <std::bfloat16_t, signed __int128> (-INT128_MAX - 1) != (std::bfloat16_t) (-INT128_MAX - 1))
    __builtin_abort ();
  if (cvt <std::bfloat16_t, unsigned __int128> (42ULL) != (std::bfloat16_t) (unsigned __int128) 42ULL
      || cvt <std::bfloat16_t, unsigned __int128> (~(unsigned __int128) 0) != (std::bfloat16_t) (~(unsigned __int128) 0))
    __builtin_abort ();
#endif
#endif
#ifdef __STDCPP_FLOAT32_T__
  if (cvt <std::float32_t, signed char> (42) != (std::float32_t) 42
      || cvt <std::float32_t, signed char> (-42) != (std::float32_t) -42
      || cvt <std::float32_t, signed char> (__SCHAR_MAX__) != (std::float32_t) __SCHAR_MAX__
      || cvt <std::float32_t, signed char> (-__SCHAR_MAX__ - 1) != (std::float32_t) (-__SCHAR_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::float32_t, unsigned char> (42) != (std::float32_t) 42
      || cvt <std::float32_t, unsigned char> ((unsigned char) ~0) != (std::float32_t) ((unsigned char) ~0))
    __builtin_abort ();
  if (cvt <std::float32_t, signed short> (42) != (std::float32_t) 42
      || cvt <std::float32_t, signed short> (-42) != (std::float32_t) -42
      || cvt <std::float32_t, signed short> (__SHRT_MAX__) != (std::float32_t) __SHRT_MAX__
      || cvt <std::float32_t, signed short> (-__SHRT_MAX__ - 1) != (std::float32_t) (-__SHRT_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::float32_t, unsigned short> (42) != (std::float32_t) 42
      || cvt <std::float32_t, unsigned short> ((unsigned short) ~0) != (std::float32_t) ((unsigned short) ~0))
    __builtin_abort ();
  if (cvt <std::float32_t, signed int> (42) != (std::float32_t) 42
      || cvt <std::float32_t, signed int> (-42) != (std::float32_t) -42
      || cvt <std::float32_t, signed int> (__INT_MAX__) != (std::float32_t) __INT_MAX__
      || cvt <std::float32_t, signed int> (-__INT_MAX__ - 1) != (std::float32_t) (-__INT_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::float32_t, unsigned int> (42) != (std::float32_t) 42U
      || cvt <std::float32_t, unsigned int> (~0U) != (std::float32_t) ~0U)
    __builtin_abort ();
  if (cvt <std::float32_t, signed long int> (42L) != (std::float32_t) 42L
      || cvt <std::float32_t, signed long int> (-42L) != (std::float32_t) -42L
      || cvt <std::float32_t, signed long int> (__LONG_MAX__) != (std::float32_t) __LONG_MAX__
      || cvt <std::float32_t, signed long int> (-__LONG_MAX__ - 1) != (std::float32_t) (-__LONG_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::float32_t, unsigned long int> (42UL) != (std::float32_t) 42UL
      || cvt <std::float32_t, unsigned long int> (~0UL) != (std::float32_t) ~0UL)
    __builtin_abort ();
  if (cvt <std::float32_t, signed long long int> (42LL) != (std::float32_t) 42LL
      || cvt <std::float32_t, signed long long int> (-42LL) != (std::float32_t) -42LL
      || cvt <std::float32_t, signed long long int> (__LONG_LONG_MAX__) != (std::float32_t) __LONG_LONG_MAX__
      || cvt <std::float32_t, signed long long int> (-__LONG_LONG_MAX__ - 1) != (std::float32_t) (-__LONG_LONG_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::float32_t, unsigned long long int> (42ULL) != (std::float32_t) 42ULL
      || cvt <std::float32_t, unsigned long long int> (~0ULL) != (std::float32_t) ~0ULL)
    __builtin_abort ();
#ifdef __SIZEOF_INT128__
  if (cvt <std::float32_t, signed __int128> (42LL) != (std::float32_t) (signed __int128) 42LL
      || cvt <std::float32_t, signed __int128> (-42LL) != (std::float32_t) (signed __int128) -42LL
      || cvt <std::float32_t, signed __int128> (INT128_MAX) != (std::float32_t) INT128_MAX
      || cvt <std::float32_t, signed __int128> (-INT128_MAX - 1) != (std::float32_t) (-INT128_MAX - 1))
    __builtin_abort ();
  if (cvt <std::float32_t, unsigned __int128> (42ULL) != (std::float32_t) (unsigned __int128) 42ULL
      || cvt <std::float32_t, unsigned __int128> (~(unsigned __int128) 0) != (std::float32_t) (~(unsigned __int128) 0))
    __builtin_abort ();
#endif
#endif
#ifdef __STDCPP_FLOAT64_T__
  if (cvt <std::float64_t, signed char> (42) != (std::float64_t) 42
      || cvt <std::float64_t, signed char> (-42) != (std::float64_t) -42
      || cvt <std::float64_t, signed char> (__SCHAR_MAX__) != (std::float64_t) __SCHAR_MAX__
      || cvt <std::float64_t, signed char> (-__SCHAR_MAX__ - 1) != (std::float64_t) (-__SCHAR_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::float64_t, unsigned char> (42) != (std::float64_t) 42
      || cvt <std::float64_t, unsigned char> ((unsigned char) ~0) != (std::float64_t) ((unsigned char) ~0))
    __builtin_abort ();
  if (cvt <std::float64_t, signed short> (42) != (std::float64_t) 42
      || cvt <std::float64_t, signed short> (-42) != (std::float64_t) -42
      || cvt <std::float64_t, signed short> (__SHRT_MAX__) != (std::float64_t) __SHRT_MAX__
      || cvt <std::float64_t, signed short> (-__SHRT_MAX__ - 1) != (std::float64_t) (-__SHRT_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::float64_t, unsigned short> (42) != (std::float64_t) 42
      || cvt <std::float64_t, unsigned short> ((unsigned short) ~0) != (std::float64_t) ((unsigned short) ~0))
    __builtin_abort ();
  if (cvt <std::float64_t, signed int> (42) != (std::float64_t) 42
      || cvt <std::float64_t, signed int> (-42) != (std::float64_t) -42
      || cvt <std::float64_t, signed int> (__INT_MAX__) != (std::float64_t) __INT_MAX__
      || cvt <std::float64_t, signed int> (-__INT_MAX__ - 1) != (std::float64_t) (-__INT_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::float64_t, unsigned int> (42) != (std::float64_t) 42U
      || cvt <std::float64_t, unsigned int> (~0U) != (std::float64_t) ~0U)
    __builtin_abort ();
  if (cvt <std::float64_t, signed long int> (42L) != (std::float64_t) 42L
      || cvt <std::float64_t, signed long int> (-42L) != (std::float64_t) -42L
      || cvt <std::float64_t, signed long int> (__LONG_MAX__) != (std::float64_t) __LONG_MAX__
      || cvt <std::float64_t, signed long int> (-__LONG_MAX__ - 1) != (std::float64_t) (-__LONG_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::float64_t, unsigned long int> (42UL) != (std::float64_t) 42UL
      || cvt <std::float64_t, unsigned long int> (~0UL) != (std::float64_t) ~0UL)
    __builtin_abort ();
  if (cvt <std::float64_t, signed long long int> (42LL) != (std::float64_t) 42LL
      || cvt <std::float64_t, signed long long int> (-42LL) != (std::float64_t) -42LL
      || cvt <std::float64_t, signed long long int> (__LONG_LONG_MAX__) != (std::float64_t) __LONG_LONG_MAX__
      || cvt <std::float64_t, signed long long int> (-__LONG_LONG_MAX__ - 1) != (std::float64_t) (-__LONG_LONG_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::float64_t, unsigned long long int> (42ULL) != (std::float64_t) 42ULL
      || cvt <std::float64_t, unsigned long long int> (~0ULL) != (std::float64_t) ~0ULL)
    __builtin_abort ();
#ifdef __SIZEOF_INT128__
  if (cvt <std::float64_t, signed __int128> (42LL) != (std::float64_t) (signed __int128) 42LL
      || cvt <std::float64_t, signed __int128> (-42LL) != (std::float64_t) (signed __int128) -42LL
      || cvt <std::float64_t, signed __int128> (INT128_MAX) != (std::float64_t) INT128_MAX
      || cvt <std::float64_t, signed __int128> (-INT128_MAX - 1) != (std::float64_t) (-INT128_MAX - 1))
    __builtin_abort ();
  if (cvt <std::float64_t, unsigned __int128> (42ULL) != (std::float64_t) (unsigned __int128) 42ULL
      || cvt <std::float64_t, unsigned __int128> (~(unsigned __int128) 0) != (std::float64_t) (~(unsigned __int128) 0))
    __builtin_abort ();
#endif
#endif
#ifdef __STDCPP_FLOAT128_T__
  if (cvt <std::float128_t, signed char> (42) != (std::float128_t) 42
      || cvt <std::float128_t, signed char> (-42) != (std::float128_t) -42
      || cvt <std::float128_t, signed char> (__SCHAR_MAX__) != (std::float128_t) __SCHAR_MAX__
      || cvt <std::float128_t, signed char> (-__SCHAR_MAX__ - 1) != (std::float128_t) (-__SCHAR_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::float128_t, unsigned char> (42) != (std::float128_t) 42
      || cvt <std::float128_t, unsigned char> ((unsigned char) ~0) != (std::float128_t) ((unsigned char) ~0))
    __builtin_abort ();
  if (cvt <std::float128_t, signed short> (42) != (std::float128_t) 42
      || cvt <std::float128_t, signed short> (-42) != (std::float128_t) -42
      || cvt <std::float128_t, signed short> (__SHRT_MAX__) != (std::float128_t) __SHRT_MAX__
      || cvt <std::float128_t, signed short> (-__SHRT_MAX__ - 1) != (std::float128_t) (-__SHRT_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::float128_t, unsigned short> (42) != (std::float128_t) 42
      || cvt <std::float128_t, unsigned short> ((unsigned short) ~0) != (std::float128_t) ((unsigned short) ~0))
    __builtin_abort ();
  if (cvt <std::float128_t, signed int> (42) != (std::float128_t) 42
      || cvt <std::float128_t, signed int> (-42) != (std::float128_t) -42
      || cvt <std::float128_t, signed int> (__INT_MAX__) != (std::float128_t) __INT_MAX__
      || cvt <std::float128_t, signed int> (-__INT_MAX__ - 1) != (std::float128_t) (-__INT_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::float128_t, unsigned int> (42) != (std::float128_t) 42U
      || cvt <std::float128_t, unsigned int> (~0U) != (std::float128_t) ~0U)
    __builtin_abort ();
  if (cvt <std::float128_t, signed long int> (42L) != (std::float128_t) 42L
      || cvt <std::float128_t, signed long int> (-42L) != (std::float128_t) -42L
      || cvt <std::float128_t, signed long int> (__LONG_MAX__) != (std::float128_t) __LONG_MAX__
      || cvt <std::float128_t, signed long int> (-__LONG_MAX__ - 1) != (std::float128_t) (-__LONG_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::float128_t, unsigned long int> (42UL) != (std::float128_t) 42UL
      || cvt <std::float128_t, unsigned long int> (~0UL) != (std::float128_t) ~0UL)
    __builtin_abort ();
  if (cvt <std::float128_t, signed long long int> (42LL) != (std::float128_t) 42LL
      || cvt <std::float128_t, signed long long int> (-42LL) != (std::float128_t) -42LL
      || cvt <std::float128_t, signed long long int> (__LONG_LONG_MAX__) != (std::float128_t) __LONG_LONG_MAX__
      || cvt <std::float128_t, signed long long int> (-__LONG_LONG_MAX__ - 1) != (std::float128_t) (-__LONG_LONG_MAX__ - 1))
    __builtin_abort ();
  if (cvt <std::float128_t, unsigned long long int> (42ULL) != (std::float128_t) 42ULL
      || cvt <std::float128_t, unsigned long long int> (~0ULL) != (std::float128_t) ~0ULL)
    __builtin_abort ();
#ifdef __SIZEOF_INT128__
  if (cvt <std::float128_t, signed __int128> (42LL) != (std::float128_t) (signed __int128) 42LL
      || cvt <std::float128_t, signed __int128> (-42LL) != (std::float128_t) (signed __int128) -42LL
      || cvt <std::float128_t, signed __int128> (INT128_MAX) != (std::float128_t) INT128_MAX
      || cvt <std::float128_t, signed __int128> (-INT128_MAX - 1) != (std::float128_t) (-INT128_MAX - 1))
    __builtin_abort ();
  if (cvt <std::float128_t, unsigned __int128> (42ULL) != (std::float128_t) (unsigned __int128) 42ULL
      || cvt <std::float128_t, unsigned __int128> (~(unsigned __int128) 0) != (std::float128_t) (~(unsigned __int128) 0))
    __builtin_abort ();
#endif
#endif

#ifdef __STDCPP_FLOAT16_T__
  if (cvt <signed char, std::float16_t> (42.0f16) != (signed char) (std::float16_t) 42.0f16
      || cvt <signed char, std::float16_t> (-42.0f16) != (signed char) (std::float16_t) -42.0f16
#if __SCHAR_MAX__ < 65504
      || cvt <signed char, std::float16_t> ((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2)) != (signed char) (std::float16_t) ((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2))
      || cvt <signed char, std::float16_t> (-((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2))) != (signed char) (std::float16_t) (-((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2)))
#endif
     )
    __builtin_abort ();
  if (cvt <unsigned char, std::float16_t> (42.0f16) != (unsigned char) (std::float16_t) 42.0f16
#if __SCHAR_MAX__ * 2 + 1 < 65504
      || cvt <unsigned char, std::float16_t> ((unsigned char) 1 << (__CHAR_BIT__ * sizeof (unsigned char) - 1)) != (unsigned char) (std::float16_t) ((unsigned char) 1 << (__CHAR_BIT__ * sizeof (unsigned char) - 1))
#endif
     )
    __builtin_abort ();
  if (cvt <signed short, std::float16_t> (42.0f16) != (signed short) (std::float16_t) 42.0f16
      || cvt <signed short, std::float16_t> (-42.0f16) != (signed short) (std::float16_t) -42.0f16
#if __SHRT_MAX__ < 65504
      || cvt <signed short, std::float16_t> ((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2)) != (signed short) (std::float16_t) ((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2))
      || cvt <signed short, std::float16_t> (-((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2))) != (signed short) (std::float16_t) (-((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2)))
#else
      || cvt <signed short, std::float16_t> (65504.0f16) != (signed short) (std::float16_t) 65504.0f16
      || cvt <signed short, std::float16_t> (-65504.0f16) != (signed short) (std::float16_t) -65504.0f16
#endif
     )
    __builtin_abort ();
  if (cvt <unsigned short, std::float16_t> (42.0f16) != (unsigned short) (std::float16_t) 42.0f16
      || cvt <unsigned short, std::float16_t> (65504.0f16) != (unsigned short) (std::float16_t) 65504.0f16)
    __builtin_abort ();
  if (cvt <signed int, std::float16_t> (42.0f16) != (signed int) (std::float16_t) 42.0f16
      || cvt <signed int, std::float16_t> (-42.0f16) != (signed int) (std::float16_t) -42.0f16
#if __INT_MAX__ < 65504
      || cvt <signed int, std::float16_t> ((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2)) != (signed int) (std::float16_t) ((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2))
      || cvt <signed int, std::float16_t> (-((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2))) != (signed int) (std::float16_t) (-((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2)))
#else
      || cvt <signed int, std::float16_t> (65504.0f16) != (signed int) (std::float16_t) 65504.0f16
      || cvt <signed int, std::float16_t> (-65504.0f16) != (signed int) (std::float16_t) -65504.0f16
#endif
     )
    __builtin_abort ();
  if (cvt <unsigned int, std::float16_t> (42.0f16) != (unsigned int) (std::float16_t) 42.0f16
      || cvt <unsigned int, std::float16_t> (65504.0f16) != (unsigned int) (std::float16_t) 65504.0f16)
    __builtin_abort ();
  if (cvt <signed long int, std::float16_t> (42.0f16) != (signed long int) (std::float16_t) 42.0f16
      || cvt <signed long int, std::float16_t> (-42.0f16) != (signed long int) (std::float16_t) -42.0f16
      || cvt <signed long int, std::float16_t> (65504.0f16) != (signed long int) (std::float16_t) 65504.0f16
      || cvt <signed long int, std::float16_t> (-65504.0f16) != (signed long int) (std::float16_t) -65504.0f16)
    __builtin_abort ();
  if (cvt <unsigned long int, std::float16_t> (42.0f16) != (unsigned long int) (std::float16_t) 42.0f16
      || cvt <unsigned long int, std::float16_t> (65504.0f16) != (unsigned long int) (std::float16_t) 65504.0f16)
    __builtin_abort ();
  if (cvt <signed long long int, std::float16_t> (42.0f16) != (signed long long int) (std::float16_t) 42.0f16
      || cvt <signed long long int, std::float16_t> (-42.0f16) != (signed long long int) (std::float16_t) -42.0f16
      || cvt <signed long long int, std::float16_t> (65504.0f16) != (signed long long int) (std::float16_t) 65504.0f16
      || cvt <signed long long int, std::float16_t> (-65504.0f16) != (signed long long int) (std::float16_t) -65504.0f16)
    __builtin_abort ();
  if (cvt <unsigned long long int, std::float16_t> (42.0f16) != (unsigned long long int) (std::float16_t) 42.0f16
      || cvt <unsigned long long int, std::float16_t> (65504.0f16) != (unsigned long long int) (std::float16_t) 65504.0f16)
    __builtin_abort ();
#ifdef __SIZEOF_INT128__
  if (cvt <signed __int128, std::float16_t> (42.0f16) != (signed __int128) (std::float16_t) 42.0f16
      || cvt <signed __int128, std::float16_t> (-42.0f16) != (signed __int128) (std::float16_t) -42.0f16
      || cvt <signed __int128, std::float16_t> (65504.0f16) != (signed __int128) (std::float16_t) 65504.0f16
      || cvt <signed __int128, std::float16_t> (-65504.0f16) != (signed __int128) (std::float16_t) -65504.0f16)
    __builtin_abort ();
  if (cvt <unsigned __int128, std::float16_t> (42.0f16) != (unsigned __int128) (std::float16_t) 42.0f16
      || cvt <unsigned __int128, std::float16_t> (65504.0f16) != (unsigned __int128) (std::float16_t) 65504.0f16)
    __builtin_abort ();
#endif
#endif
#ifdef __STDCPP_BFLOAT16_T__
  if (cvt <signed char, std::bfloat16_t> (42.0bf16) != (signed char) (std::bfloat16_t) 42.0bf16
      || cvt <signed char, std::bfloat16_t> (-42.0bf16) != (signed char) (std::bfloat16_t) -42.0bf16
      || cvt <signed char, std::bfloat16_t> ((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2)) != (signed char) (std::bfloat16_t) ((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2))
      || cvt <signed char, std::bfloat16_t> (-((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2))) != (signed char) (std::bfloat16_t) (-((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2))))
    __builtin_abort ();
  if (cvt <unsigned char, std::bfloat16_t> (42.0bf16) != (unsigned char) (std::bfloat16_t) 42.0bf16
      || cvt <unsigned char, std::bfloat16_t> ((unsigned char) 1 << (__CHAR_BIT__ * sizeof (unsigned char) - 1)) != (unsigned char) (std::bfloat16_t) ((unsigned char) 1 << (__CHAR_BIT__ * sizeof (unsigned char) - 1)))
    __builtin_abort ();
  if (cvt <signed short, std::bfloat16_t> (42.0bf16) != (signed short) (std::bfloat16_t) 42.0bf16
      || cvt <signed short, std::bfloat16_t> (-42.0bf16) != (signed short) (std::bfloat16_t) -42.0bf16
      || cvt <signed short, std::bfloat16_t> ((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2)) != (signed short) (std::bfloat16_t) ((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2))
      || cvt <signed short, std::bfloat16_t> (-((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2))) != (signed short) (std::bfloat16_t) (-((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2))))
    __builtin_abort ();
  if (cvt <unsigned short, std::bfloat16_t> (42.0bf16) != (unsigned short) (std::bfloat16_t) 42.0bf16
      || cvt <unsigned short, std::bfloat16_t> ((unsigned short) 1 << (__CHAR_BIT__ * sizeof (unsigned short) - 1)) != (unsigned short) (std::bfloat16_t) ((unsigned short) 1 << (__CHAR_BIT__ * sizeof (unsigned short) - 1)))
    __builtin_abort ();
  if (cvt <signed int, std::bfloat16_t> (42.0bf16) != (signed int) (std::bfloat16_t) 42.0bf16
      || cvt <signed int, std::bfloat16_t> (-42.0bf16) != (signed int) (std::bfloat16_t) -42.0bf16
      || cvt <signed int, std::bfloat16_t> ((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2)) != (signed int) (std::bfloat16_t) ((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2))
      || cvt <signed int, std::bfloat16_t> (-((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2))) != (signed int) (std::bfloat16_t) (-((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2))))
    __builtin_abort ();
  if (cvt <unsigned int, std::bfloat16_t> (42.0bf16) != (unsigned int) (std::bfloat16_t) 42.0bf16
      || cvt <unsigned int, std::bfloat16_t> ((unsigned int) 1 << (__CHAR_BIT__ * sizeof (unsigned int) - 1)) != (unsigned int) (std::bfloat16_t) ((unsigned int) 1 << (__CHAR_BIT__ * sizeof (unsigned int) - 1)))
    __builtin_abort ();
  if (cvt <signed long int, std::bfloat16_t> (42.0bf16) != (signed long int) (std::bfloat16_t) 42.0bf16
      || cvt <signed long int, std::bfloat16_t> (-42.0bf16) != (signed long int) (std::bfloat16_t) -42.0bf16
      || cvt <signed long int, std::bfloat16_t> ((signed long int) 1 << (__CHAR_BIT__ * sizeof (signed long int) - 2)) != (signed long int) (std::bfloat16_t) ((signed long int) 1 << (__CHAR_BIT__ * sizeof (signed long int) - 2))
      || cvt <signed long int, std::bfloat16_t> (-((signed long int) 1 << (__CHAR_BIT__ * sizeof (signed long int) - 2))) != (signed long int) (std::bfloat16_t) (-((signed long int) 1 << (__CHAR_BIT__ * sizeof (signed long int) - 2))))
    __builtin_abort ();
  if (cvt <unsigned long int, std::bfloat16_t> (42.0bf16) != (unsigned long int) (std::bfloat16_t) 42.0bf16
      || cvt <unsigned long int, std::bfloat16_t> ((unsigned long int) 1 << (__CHAR_BIT__ * sizeof (unsigned long int) - 1)) != (unsigned long int) (std::bfloat16_t) ((unsigned long int) 1 << (__CHAR_BIT__ * sizeof (unsigned long int) - 1)))
    __builtin_abort ();
  if (cvt <signed long long int, std::bfloat16_t> (42.0bf16) != (signed long long int) (std::bfloat16_t) 42.0bf16
      || cvt <signed long long int, std::bfloat16_t> (-42.0bf16) != (signed long long int) (std::bfloat16_t) -42.0bf16
      || cvt <signed long long int, std::bfloat16_t> ((signed long long int) 1 << (__CHAR_BIT__ * sizeof (signed long long int) - 2)) != (signed long long int) (std::bfloat16_t) ((signed long long int) 1 << (__CHAR_BIT__ * sizeof (signed long long int) - 2))
      || cvt <signed long long int, std::bfloat16_t> (-((signed long long int) 1 << (__CHAR_BIT__ * sizeof (signed long long int) - 2))) != (signed long long int) (std::bfloat16_t) (-((signed long long int) 1 << (__CHAR_BIT__ * sizeof (signed long long int) - 2))))
    __builtin_abort ();
  if (cvt <unsigned long long int, std::bfloat16_t> (42.0bf16) != (unsigned long long int) (std::bfloat16_t) 42.0bf16
      || cvt <unsigned long long int, std::bfloat16_t> ((unsigned long long int) 1 << (__CHAR_BIT__ * sizeof (unsigned long long int) - 1)) != (unsigned long long int) (std::bfloat16_t) ((unsigned long long int) 1 << (__CHAR_BIT__ * sizeof (unsigned long long int) - 1)))
    __builtin_abort ();
#ifdef __SIZEOF_INT128__
  if (cvt <signed __int128, std::bfloat16_t> (42.0bf16) != (signed __int128) (std::bfloat16_t) 42.0bf16
      || cvt <signed __int128, std::bfloat16_t> (-42.0bf16) != (signed __int128) (std::bfloat16_t) -42.0bf16
      || cvt <signed __int128, std::bfloat16_t> ((signed __int128) 1 << (__CHAR_BIT__ * sizeof (signed __int128) - 2)) != (signed __int128) (std::bfloat16_t) ((signed __int128) 1 << (__CHAR_BIT__ * sizeof (signed __int128) - 2))
      || cvt <signed __int128, std::bfloat16_t> (-((signed __int128) 1 << (__CHAR_BIT__ * sizeof (signed __int128) - 2))) != (signed __int128) (std::bfloat16_t) (-((signed __int128) 1 << (__CHAR_BIT__ * sizeof (signed __int128) - 2))))
    __builtin_abort ();
  if (cvt <unsigned __int128, std::bfloat16_t> (42.0bf16) != (unsigned __int128) (std::bfloat16_t) 42.0bf16
      || cvt <unsigned __int128, std::bfloat16_t> ((unsigned __int128) 1 << (__CHAR_BIT__ * sizeof (unsigned __int128) - 1)) != (unsigned __int128) (std::bfloat16_t) ((unsigned __int128) 1 << (__CHAR_BIT__ * sizeof (unsigned __int128) - 1)))
    __builtin_abort ();
#endif
#endif
#ifdef __STDCPP_FLOAT32_T__
  if (cvt <signed char, std::float32_t> (42.0f32) != (signed char) (std::float32_t) 42.0f32
      || cvt <signed char, std::float32_t> (-42.0f32) != (signed char) (std::float32_t) -42.0f32
      || cvt <signed char, std::float32_t> ((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2)) != (signed char) (std::float32_t) ((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2))
      || cvt <signed char, std::float32_t> (-((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2))) != (signed char) (std::float32_t) (-((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2))))
    __builtin_abort ();
  if (cvt <unsigned char, std::float32_t> (42.0f32) != (unsigned char) (std::float32_t) 42.0f32
      || cvt <unsigned char, std::float32_t> ((unsigned char) 1 << (__CHAR_BIT__ * sizeof (unsigned char) - 1)) != (unsigned char) (std::float32_t) ((unsigned char) 1 << (__CHAR_BIT__ * sizeof (unsigned char) - 1)))
    __builtin_abort ();
  if (cvt <signed short, std::float32_t> (42.0f32) != (signed short) (std::float32_t) 42.0f32
      || cvt <signed short, std::float32_t> (-42.0f32) != (signed short) (std::float32_t) -42.0f32
      || cvt <signed short, std::float32_t> ((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2)) != (signed short) (std::float32_t) ((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2))
      || cvt <signed short, std::float32_t> (-((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2))) != (signed short) (std::float32_t) (-((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2))))
    __builtin_abort ();
  if (cvt <unsigned short, std::float32_t> (42.0f32) != (unsigned short) (std::float32_t) 42.0f32
      || cvt <unsigned short, std::float32_t> ((unsigned short) 1 << (__CHAR_BIT__ * sizeof (unsigned short) - 1)) != (unsigned short) (std::float32_t) ((unsigned short) 1 << (__CHAR_BIT__ * sizeof (unsigned short) - 1)))
    __builtin_abort ();
  if (cvt <signed int, std::float32_t> (42.0f32) != (signed int) (std::float32_t) 42.0f32
      || cvt <signed int, std::float32_t> (-42.0f32) != (signed int) (std::float32_t) -42.0f32
      || cvt <signed int, std::float32_t> ((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2)) != (signed int) (std::float32_t) ((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2))
      || cvt <signed int, std::float32_t> (-((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2))) != (signed int) (std::float32_t) (-((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2))))
    __builtin_abort ();
  if (cvt <unsigned int, std::float32_t> (42.0f32) != (unsigned int) (std::float32_t) 42.0f32
      || cvt <unsigned int, std::float32_t> ((unsigned int) 1 << (__CHAR_BIT__ * sizeof (unsigned int) - 1)) != (unsigned int) (std::float32_t) ((unsigned int) 1 << (__CHAR_BIT__ * sizeof (unsigned int) - 1)))
    __builtin_abort ();
  if (cvt <signed long int, std::float32_t> (42.0f32) != (signed long int) (std::float32_t) 42.0f32
      || cvt <signed long int, std::float32_t> (-42.0f32) != (signed long int) (std::float32_t) -42.0f32
      || cvt <signed long int, std::float32_t> ((signed long int) 1 << (__CHAR_BIT__ * sizeof (signed long int) - 2)) != (signed long int) (std::float32_t) ((signed long int) 1 << (__CHAR_BIT__ * sizeof (signed long int) - 2))
      || cvt <signed long int, std::float32_t> (-((signed long int) 1 << (__CHAR_BIT__ * sizeof (signed long int) - 2))) != (signed long int) (std::float32_t) (-((signed long int) 1 << (__CHAR_BIT__ * sizeof (signed long int) - 2))))
    __builtin_abort ();
  if (cvt <unsigned long int, std::float32_t> (42.0f32) != (unsigned long int) (std::float32_t) 42.0f32
      || cvt <unsigned long int, std::float32_t> ((unsigned long int) 1 << (__CHAR_BIT__ * sizeof (unsigned long int) - 1)) != (unsigned long int) (std::float32_t) ((unsigned long int) 1 << (__CHAR_BIT__ * sizeof (unsigned long int) - 1)))
    __builtin_abort ();
  if (cvt <signed long long int, std::float32_t> (42.0f32) != (signed long long int) (std::float32_t) 42.0f32
      || cvt <signed long long int, std::float32_t> (-42.0f32) != (signed long long int) (std::float32_t) -42.0f32
      || cvt <signed long long int, std::float32_t> ((signed long long int) 1 << (__CHAR_BIT__ * sizeof (signed long long int) - 2)) != (signed long long int) (std::float32_t) ((signed long long int) 1 << (__CHAR_BIT__ * sizeof (signed long long int) - 2))
      || cvt <signed long long int, std::float32_t> (-((signed long long int) 1 << (__CHAR_BIT__ * sizeof (signed long long int) - 2))) != (signed long long int) (std::float32_t) (-((signed long long int) 1 << (__CHAR_BIT__ * sizeof (signed long long int) - 2))))
    __builtin_abort ();
  if (cvt <unsigned long long int, std::float32_t> (42.0f32) != (unsigned long long int) (std::float32_t) 42.0f32
      || cvt <unsigned long long int, std::float32_t> ((unsigned long long int) 1 << (__CHAR_BIT__ * sizeof (unsigned long long int) - 1)) != (unsigned long long int) (std::float32_t) ((unsigned long long int) 1 << (__CHAR_BIT__ * sizeof (unsigned long long int) - 1)))
    __builtin_abort ();
#ifdef __SIZEOF_INT128__
  if (cvt <signed __int128, std::float32_t> (42.0f32) != (signed __int128) (std::float32_t) 42.0f32
      || cvt <signed __int128, std::float32_t> (-42.0f32) != (signed __int128) (std::float32_t) -42.0f32
      || cvt <signed __int128, std::float32_t> ((signed __int128) 1 << (__CHAR_BIT__ * sizeof (signed __int128) - 2)) != (signed __int128) (std::float32_t) ((signed __int128) 1 << (__CHAR_BIT__ * sizeof (signed __int128) - 2))
      || cvt <signed __int128, std::float32_t> (-((signed __int128) 1 << (__CHAR_BIT__ * sizeof (signed __int128) - 2))) != (signed __int128) (std::float32_t) (-((signed __int128) 1 << (__CHAR_BIT__ * sizeof (signed __int128) - 2))))
    __builtin_abort ();
  if (cvt <unsigned __int128, std::float32_t> (42.0f32) != (unsigned __int128) (std::float32_t) 42.0f32
      || cvt <unsigned __int128, std::float32_t> ((unsigned __int128) 1 << (__CHAR_BIT__ * sizeof (unsigned __int128) - 1)) != (unsigned __int128) (std::float32_t) ((unsigned __int128) 1 << (__CHAR_BIT__ * sizeof (unsigned __int128) - 1)))
    __builtin_abort ();
#endif
#endif
#ifdef __STDCPP_FLOAT64_T__
  if (cvt <signed char, std::float64_t> (42.0f64) != (signed char) (std::float64_t) 42.0f64
      || cvt <signed char, std::float64_t> (-42.0f64) != (signed char) (std::float64_t) -42.0f64
      || cvt <signed char, std::float64_t> ((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2)) != (signed char) (std::float64_t) ((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2))
      || cvt <signed char, std::float64_t> (-((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2))) != (signed char) (std::float64_t) (-((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2))))
    __builtin_abort ();
  if (cvt <unsigned char, std::float64_t> (42.0f64) != (unsigned char) (std::float64_t) 42.0f64
      || cvt <unsigned char, std::float64_t> ((unsigned char) 1 << (__CHAR_BIT__ * sizeof (unsigned char) - 1)) != (unsigned char) (std::float64_t) ((unsigned char) 1 << (__CHAR_BIT__ * sizeof (unsigned char) - 1)))
    __builtin_abort ();
  if (cvt <signed short, std::float64_t> (42.0f64) != (signed short) (std::float64_t) 42.0f64
      || cvt <signed short, std::float64_t> (-42.0f64) != (signed short) (std::float64_t) -42.0f64
      || cvt <signed short, std::float64_t> ((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2)) != (signed short) (std::float64_t) ((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2))
      || cvt <signed short, std::float64_t> (-((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2))) != (signed short) (std::float64_t) (-((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2))))
    __builtin_abort ();
  if (cvt <unsigned short, std::float64_t> (42.0f64) != (unsigned short) (std::float64_t) 42.0f64
      || cvt <unsigned short, std::float64_t> ((unsigned short) 1 << (__CHAR_BIT__ * sizeof (unsigned short) - 1)) != (unsigned short) (std::float64_t) ((unsigned short) 1 << (__CHAR_BIT__ * sizeof (unsigned short) - 1)))
    __builtin_abort ();
  if (cvt <signed int, std::float64_t> (42.0f64) != (signed int) (std::float64_t) 42.0f64
      || cvt <signed int, std::float64_t> (-42.0f64) != (signed int) (std::float64_t) -42.0f64
      || cvt <signed int, std::float64_t> ((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2)) != (signed int) (std::float64_t) ((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2))
      || cvt <signed int, std::float64_t> (-((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2))) != (signed int) (std::float64_t) (-((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2))))
    __builtin_abort ();
  if (cvt <unsigned int, std::float64_t> (42.0f64) != (unsigned int) (std::float64_t) 42.0f64
      || cvt <unsigned int, std::float64_t> ((unsigned int) 1 << (__CHAR_BIT__ * sizeof (unsigned int) - 1)) != (unsigned int) (std::float64_t) ((unsigned int) 1 << (__CHAR_BIT__ * sizeof (unsigned int) - 1)))
    __builtin_abort ();
  if (cvt <signed long int, std::float64_t> (42.0f64) != (signed long int) (std::float64_t) 42.0f64
      || cvt <signed long int, std::float64_t> (-42.0f64) != (signed long int) (std::float64_t) -42.0f64
      || cvt <signed long int, std::float64_t> ((signed long int) 1 << (__CHAR_BIT__ * sizeof (signed long int) - 2)) != (signed long int) (std::float64_t) ((signed long int) 1 << (__CHAR_BIT__ * sizeof (signed long int) - 2))
      || cvt <signed long int, std::float64_t> (-((signed long int) 1 << (__CHAR_BIT__ * sizeof (signed long int) - 2))) != (signed long int) (std::float64_t) (-((signed long int) 1 << (__CHAR_BIT__ * sizeof (signed long int) - 2))))
    __builtin_abort ();
  if (cvt <unsigned long int, std::float64_t> (42.0f64) != (unsigned long int) (std::float64_t) 42.0f64
      || cvt <unsigned long int, std::float64_t> ((unsigned long int) 1 << (__CHAR_BIT__ * sizeof (unsigned long int) - 1)) != (unsigned long int) (std::float64_t) ((unsigned long int) 1 << (__CHAR_BIT__ * sizeof (unsigned long int) - 1)))
    __builtin_abort ();
  if (cvt <signed long long int, std::float64_t> (42.0f64) != (signed long long int) (std::float64_t) 42.0f64
      || cvt <signed long long int, std::float64_t> (-42.0f64) != (signed long long int) (std::float64_t) -42.0f64
      || cvt <signed long long int, std::float64_t> ((signed long long int) 1 << (__CHAR_BIT__ * sizeof (signed long long int) - 2)) != (signed long long int) (std::float64_t) ((signed long long int) 1 << (__CHAR_BIT__ * sizeof (signed long long int) - 2))
      || cvt <signed long long int, std::float64_t> (-((signed long long int) 1 << (__CHAR_BIT__ * sizeof (signed long long int) - 2))) != (signed long long int) (std::float64_t) (-((signed long long int) 1 << (__CHAR_BIT__ * sizeof (signed long long int) - 2))))
    __builtin_abort ();
  if (cvt <unsigned long long int, std::float64_t> (42.0f64) != (unsigned long long int) (std::float64_t) 42.0f64
      || cvt <unsigned long long int, std::float64_t> ((unsigned long long int) 1 << (__CHAR_BIT__ * sizeof (unsigned long long int) - 1)) != (unsigned long long int) (std::float64_t) ((unsigned long long int) 1 << (__CHAR_BIT__ * sizeof (unsigned long long int) - 1)))
    __builtin_abort ();
#ifdef __SIZEOF_INT128__
  if (cvt <signed __int128, std::float64_t> (42.0f64) != (signed __int128) (std::float64_t) 42.0f64
      || cvt <signed __int128, std::float64_t> (-42.0f64) != (signed __int128) (std::float64_t) -42.0f64
      || cvt <signed __int128, std::float64_t> ((signed __int128) 1 << (__CHAR_BIT__ * sizeof (signed __int128) - 2)) != (signed __int128) (std::float64_t) ((signed __int128) 1 << (__CHAR_BIT__ * sizeof (signed __int128) - 2))
      || cvt <signed __int128, std::float64_t> (-((signed __int128) 1 << (__CHAR_BIT__ * sizeof (signed __int128) - 2))) != (signed __int128) (std::float64_t) (-((signed __int128) 1 << (__CHAR_BIT__ * sizeof (signed __int128) - 2))))
    __builtin_abort ();
  if (cvt <unsigned __int128, std::float64_t> (42.0f64) != (unsigned __int128) (std::float64_t) 42.0f64
      || cvt <unsigned __int128, std::float64_t> ((unsigned __int128) 1 << (__CHAR_BIT__ * sizeof (unsigned __int128) - 1)) != (unsigned __int128) (std::float64_t) ((unsigned __int128) 1 << (__CHAR_BIT__ * sizeof (unsigned __int128) - 1)))
    __builtin_abort ();
#endif
#endif
#ifdef __STDCPP_FLOAT128_T__
  if (cvt <signed char, std::float128_t> (42.0f128) != (signed char) (std::float128_t) 42.0f128
      || cvt <signed char, std::float128_t> (-42.0f128) != (signed char) (std::float128_t) -42.0f128
      || cvt <signed char, std::float128_t> ((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2)) != (signed char) (std::float128_t) ((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2))
      || cvt <signed char, std::float128_t> (-((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2))) != (signed char) (std::float128_t) (-((signed char) 1 << (__CHAR_BIT__ * sizeof (signed char) - 2))))
    __builtin_abort ();
  if (cvt <unsigned char, std::float128_t> (42.0f128) != (unsigned char) (std::float128_t) 42.0f128
      || cvt <unsigned char, std::float128_t> ((unsigned char) 1 << (__CHAR_BIT__ * sizeof (unsigned char) - 1)) != (unsigned char) (std::float128_t) ((unsigned char) 1 << (__CHAR_BIT__ * sizeof (unsigned char) - 1)))
    __builtin_abort ();
  if (cvt <signed short, std::float128_t> (42.0f128) != (signed short) (std::float128_t) 42.0f128
      || cvt <signed short, std::float128_t> (-42.0f128) != (signed short) (std::float128_t) -42.0f128
      || cvt <signed short, std::float128_t> ((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2)) != (signed short) (std::float128_t) ((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2))
      || cvt <signed short, std::float128_t> (-((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2))) != (signed short) (std::float128_t) (-((signed short) 1 << (__CHAR_BIT__ * sizeof (signed short) - 2))))
    __builtin_abort ();
  if (cvt <unsigned short, std::float128_t> (42.0f128) != (unsigned short) (std::float128_t) 42.0f128
      || cvt <unsigned short, std::float128_t> ((unsigned short) 1 << (__CHAR_BIT__ * sizeof (unsigned short) - 1)) != (unsigned short) (std::float128_t) ((unsigned short) 1 << (__CHAR_BIT__ * sizeof (unsigned short) - 1)))
    __builtin_abort ();
  if (cvt <signed int, std::float128_t> (42.0f128) != (signed int) (std::float128_t) 42.0f128
      || cvt <signed int, std::float128_t> (-42.0f128) != (signed int) (std::float128_t) -42.0f128
      || cvt <signed int, std::float128_t> ((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2)) != (signed int) (std::float128_t) ((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2))
      || cvt <signed int, std::float128_t> (-((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2))) != (signed int) (std::float128_t) (-((signed int) 1 << (__CHAR_BIT__ * sizeof (signed int) - 2))))
    __builtin_abort ();
  if (cvt <unsigned int, std::float128_t> (42.0f128) != (unsigned int) (std::float128_t) 42.0f128
      || cvt <unsigned int, std::float128_t> ((unsigned int) 1 << (__CHAR_BIT__ * sizeof (unsigned int) - 1)) != (unsigned int) (std::float128_t) ((unsigned int) 1 << (__CHAR_BIT__ * sizeof (unsigned int) - 1)))
    __builtin_abort ();
  if (cvt <signed long int, std::float128_t> (42.0f128) != (signed long int) (std::float128_t) 42.0f128
      || cvt <signed long int, std::float128_t> (-42.0f128) != (signed long int) (std::float128_t) -42.0f128
      || cvt <signed long int, std::float128_t> ((signed long int) 1 << (__CHAR_BIT__ * sizeof (signed long int) - 2)) != (signed long int) (std::float128_t) ((signed long int) 1 << (__CHAR_BIT__ * sizeof (signed long int) - 2))
      || cvt <signed long int, std::float128_t> (-((signed long int) 1 << (__CHAR_BIT__ * sizeof (signed long int) - 2))) != (signed long int) (std::float128_t) (-((signed long int) 1 << (__CHAR_BIT__ * sizeof (signed long int) - 2))))
    __builtin_abort ();
  if (cvt <unsigned long int, std::float128_t> (42.0f128) != (unsigned long int) (std::float128_t) 42.0f128
      || cvt <unsigned long int, std::float128_t> ((unsigned long int) 1 << (__CHAR_BIT__ * sizeof (unsigned long int) - 1)) != (unsigned long int) (std::float128_t) ((unsigned long int) 1 << (__CHAR_BIT__ * sizeof (unsigned long int) - 1)))
    __builtin_abort ();
  if (cvt <signed long long int, std::float128_t> (42.0f128) != (signed long long int) (std::float128_t) 42.0f128
      || cvt <signed long long int, std::float128_t> (-42.0f128) != (signed long long int) (std::float128_t) -42.0f128
      || cvt <signed long long int, std::float128_t> ((signed long long int) 1 << (__CHAR_BIT__ * sizeof (signed long long int) - 2)) != (signed long long int) (std::float128_t) ((signed long long int) 1 << (__CHAR_BIT__ * sizeof (signed long long int) - 2))
      || cvt <signed long long int, std::float128_t> (-((signed long long int) 1 << (__CHAR_BIT__ * sizeof (signed long long int) - 2))) != (signed long long int) (std::float128_t) (-((signed long long int) 1 << (__CHAR_BIT__ * sizeof (signed long long int) - 2))))
    __builtin_abort ();
  if (cvt <unsigned long long int, std::float128_t> (42.0f128) != (unsigned long long int) (std::float128_t) 42.0f128
      || cvt <unsigned long long int, std::float128_t> ((unsigned long long int) 1 << (__CHAR_BIT__ * sizeof (unsigned long long int) - 1)) != (unsigned long long int) (std::float128_t) ((unsigned long long int) 1 << (__CHAR_BIT__ * sizeof (unsigned long long int) - 1)))
    __builtin_abort ();
#ifdef __SIZEOF_INT128__
  if (cvt <signed __int128, std::float128_t> (42.0f128) != (signed __int128) (std::float128_t) 42.0f128
      || cvt <signed __int128, std::float128_t> (-42.0f128) != (signed __int128) (std::float128_t) -42.0f128
      || cvt <signed __int128, std::float128_t> ((signed __int128) 1 << (__CHAR_BIT__ * sizeof (signed __int128) - 2)) != (signed __int128) (std::float128_t) ((signed __int128) 1 << (__CHAR_BIT__ * sizeof (signed __int128) - 2))
      || cvt <signed __int128, std::float128_t> (-((signed __int128) 1 << (__CHAR_BIT__ * sizeof (signed __int128) - 2))) != (signed __int128) (std::float128_t) (-((signed __int128) 1 << (__CHAR_BIT__ * sizeof (signed __int128) - 2))))
    __builtin_abort ();
  if (cvt <unsigned __int128, std::float128_t> (42.0f128) != (unsigned __int128) (std::float128_t) 42.0f128
      || cvt <unsigned __int128, std::float128_t> ((unsigned __int128) 1 << (__CHAR_BIT__ * sizeof (unsigned __int128) - 1)) != (unsigned __int128) (std::float128_t) ((unsigned __int128) 1 << (__CHAR_BIT__ * sizeof (unsigned __int128) - 1)))
    __builtin_abort ();
#endif
#endif
#endif
#endif
}
