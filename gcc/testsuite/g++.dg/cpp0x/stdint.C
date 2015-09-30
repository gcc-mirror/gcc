// PR c++/52764
// { dg-do compile { target c++11 } }
// { dg-require-effective-target stdint_types }

#include <stdint.h>

#ifdef __INT8_TYPE__
# if (!defined INT8_MAX \
      || !defined INT8_MIN)
# error
# endif
#endif
#ifdef __UINT8_TYPE__
# if !defined UINT8_MAX
# error
# endif
#endif
#ifdef __INT16_TYPE__
# if (!defined INT16_MAX \
      || !defined INT16_MIN)
# error
# endif
#endif
#ifdef __UINT16_TYPE__
# if !defined UINT16_MAX
# error
# endif
#endif
#ifdef __INT32_TYPE__
# if (!defined INT32_MAX \
      || !defined INT32_MIN)
# error
# endif
#endif
#ifdef __UINT32_TYPE__
# if !defined UINT32_MAX
# error
# endif
#endif
#ifdef __INT64_TYPE__
# if (!defined INT64_MAX \
      || !defined INT64_MIN)
# error
# endif
#endif
#ifdef __UINT64_TYPE__
# if !defined UINT64_MAX
# error
# endif
#endif

#if (!defined INT_LEAST8_MAX \
     || !defined INT_LEAST8_MIN	\
     || !defined UINT_LEAST8_MAX \
     || !defined INT_LEAST16_MAX \
     || !defined INT_LEAST16_MIN \
     || !defined UINT_LEAST16_MAX \
     || !defined INT_LEAST32_MAX \
     || !defined INT_LEAST32_MIN \
     || !defined UINT_LEAST32_MAX \
     || !defined INT_LEAST64_MAX \
     || !defined INT_LEAST64_MIN \
     || !defined UINT_LEAST64_MAX)
#error
#endif

#if (!defined INT_FAST8_MAX \
     || !defined INT_FAST8_MIN \
     || !defined UINT_FAST8_MAX \
     || !defined INT_FAST16_MAX	\
     || !defined INT_FAST16_MIN	\
     || !defined UINT_FAST16_MAX \
     || !defined INT_FAST32_MAX	\
     || !defined INT_FAST32_MIN	\
     || !defined UINT_FAST32_MAX \
     || !defined INT_FAST64_MAX	\
     || !defined INT_FAST64_MIN	\
     || !defined UINT_FAST64_MAX)
#error
#endif

#ifdef __INTPTR_TYPE__
# if (!defined INTPTR_MAX \
      || !defined INTPTR_MIN)
# error
# endif
#endif
#ifdef __UINTPTR_TYPE__
# if !defined UINTPTR_MAX
# error
# endif
#endif

#if (!defined INTMAX_MAX \
     || !defined INTMAX_MIN \
     || !defined UINTMAX_MAX)
#error
#endif

#if (!defined PTRDIFF_MAX \
     || !defined PTRDIFF_MIN)
#error
#endif

#if (!defined SIG_ATOMIC_MAX \
     || !defined SIG_ATOMIC_MIN)
#error
#endif

#if !defined SIZE_MAX
#error
#endif

#if (!defined WCHAR_MAX \
     || !defined WCHAR_MIN)
#error
#endif

#if (!defined WINT_MAX \
     || !defined WINT_MIN)
#error
#endif

#if (!defined INT8_C \
     || !defined INT16_C \
     || !defined INT32_C \
     || !defined INT64_C \
     || !defined UINT8_C \
     || !defined UINT16_C \
     || !defined UINT32_C \
     || !defined UINT64_C \
     || !defined INTMAX_C \
     || !defined UINTMAX_C)
#error
#endif
