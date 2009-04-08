/* Verify that the limits defined in <stdint.h> are those GCC expects
   internally to be defined and that they are usable in #if
   conditions.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -fhosted" } */

#include <stdint.h>

/* Exact-width and pointer-holding types are optional.  */
#if defined(INT8_MIN) != defined(__INT8_TYPE__)
#error "Unexpected INT8_MIN definedness"
#endif
#if defined(INT8_MAX) != defined(__INT8_TYPE__)
#error "Unexpected INT8_MAX definedness"
#endif
#if defined(UINT8_MAX) != defined(__UINT8_TYPE__)
#error "Unexpected UINT8_MAX definedness"
#endif
#if defined(INT16_MIN) != defined(__INT16_TYPE__)
#error "Unexpected INT16_MIN definedness"
#endif
#if defined(INT16_MAX) != defined(__INT16_TYPE__)
#error "Unexpected INT16_MAX definedness"
#endif
#if defined(UINT16_MAX) != defined(__UINT16_TYPE__)
#error "Unexpected UINT16_MAX definedness"
#endif
#if defined(INT32_MIN) != defined(__INT32_TYPE__)
#error "Unexpected INT32_MIN definedness"
#endif
#if defined(INT32_MAX) != defined(__INT32_TYPE__)
#error "Unexpected INT32_MAX definedness"
#endif
#if defined(UINT32_MAX) != defined(__UINT32_TYPE__)
#error "Unexpected UINT32_MAX definedness"
#endif
#if defined(INT64_MIN) != defined(__INT64_TYPE__)
#error "Unexpected INT64_MIN definedness"
#endif
#if defined(INT64_MAX) != defined(__INT64_TYPE__)
#error "Unexpected INT64_MAX definedness"
#endif
#if defined(UINT64_MAX) != defined(__UINT64_TYPE__)
#error "Unexpected UINT64_MAX definedness"
#endif
#if defined(INTPTR_MIN) != defined(__INTPTR_TYPE__)
#error "Unexpected INTPTR_MIN definedness"
#endif
#if defined(INTPTR_MAX) != defined(__INTPTR_TYPE__)
#error "Unexpected INTPTR_MAX definedness"
#endif
#if defined(UINTPTR_MAX) != defined(__UINTPTR_TYPE__)
#error "Unexpected UINTPTR_MAX definedness"
#endif

#if defined(INT8_MIN) && INT8_MIN != -__INT8_MAX__-1
#error "INT8_MIN not usable in #if or wrong value"
#endif
#if defined(INT8_MAX) && INT8_MAX != __INT8_MAX__
#error "INT8_MAX not usable in #if or wrong value"
#endif
#if defined(UINT8_MAX) && UINT8_MAX != __UINT8_MAX__
#error "UINT8_MAX not usable in #if or wrong value"
#endif
#if defined(INT16_MIN) && INT16_MIN != -__INT16_MAX__-1
#error "INT16_MIN not usable in #if or wrong value"
#endif
#if defined(INT16_MAX) && INT16_MAX != __INT16_MAX__
#error "INT16_MAX not usable in #if or wrong value"
#endif
#if defined(UINT16_MAX) && UINT16_MAX != __UINT16_MAX__
#error "UINT16_MAX not usable in #if or wrong value"
#endif
#if defined(INT32_MIN) && INT32_MIN != -__INT32_MAX__-1
#error "INT32_MIN not usable in #if or wrong value"
#endif
#if defined(INT32_MAX) && INT32_MAX != __INT32_MAX__
#error "INT32_MAX not usable in #if or wrong value"
#endif
#if defined(UINT32_MAX) && UINT32_MAX != __UINT32_MAX__
#error "UINT32_MAX not usable in #if or wrong value"
#endif
#if defined(INT64_MIN) && INT64_MIN != -__INT64_MAX__-1
#error "INT64_MIN not usable in #if or wrong value"
#endif
#if defined(INT64_MAX) && INT64_MAX != __INT64_MAX__
#error "INT64_MAX not usable in #if or wrong value"
#endif
#if defined(UINT64_MAX) && UINT64_MAX != __UINT64_MAX__
#error "UINT64_MAX not usable in #if or wrong value"
#endif

#if INT_LEAST8_MIN != -__INT_LEAST8_MAX__-1
#error "INT_LEAST8_MIN not usable in #if or wrong value"
#endif
#if INT_LEAST8_MAX != __INT_LEAST8_MAX__
#error "INT_LEAST8_MAX not usable in #if or wrong value"
#endif
#if UINT_LEAST8_MAX != __UINT_LEAST8_MAX__
#error "UINT_LEAST8_MAX not usable in #if or wrong value"
#endif
#if INT_LEAST16_MIN != -__INT_LEAST16_MAX__-1
#error "INT_LEAST16_MIN not usable in #if or wrong value"
#endif
#if INT_LEAST16_MAX != __INT_LEAST16_MAX__
#error "INT_LEAST16_MAX not usable in #if or wrong value"
#endif
#if UINT_LEAST16_MAX != __UINT_LEAST16_MAX__
#error "UINT_LEAST16_MAX not usable in #if or wrong value"
#endif
#if INT_LEAST32_MIN != -__INT_LEAST32_MAX__-1
#error "INT_LEAST32_MIN not usable in #if or wrong value"
#endif
#if INT_LEAST32_MAX != __INT_LEAST32_MAX__
#error "INT_LEAST32_MAX not usable in #if or wrong value"
#endif
#if UINT_LEAST32_MAX != __UINT_LEAST32_MAX__
#error "UINT_LEAST32_MAX not usable in #if or wrong value"
#endif
#if INT_LEAST64_MIN != -__INT_LEAST64_MAX__-1
#error "INT_LEAST64_MIN not usable in #if or wrong value"
#endif
#if INT_LEAST64_MAX != __INT_LEAST64_MAX__
#error "INT_LEAST64_MAX not usable in #if or wrong value"
#endif
#if UINT_LEAST64_MAX != __UINT_LEAST64_MAX__
#error "UINT_LEAST64_MAX not usable in #if or wrong value"
#endif

#if INT_FAST8_MIN != -__INT_FAST8_MAX__-1
#error "INT_FAST8_MIN not usable in #if or wrong value"
#endif
#if INT_FAST8_MAX != __INT_FAST8_MAX__
#error "INT_FAST8_MAX not usable in #if or wrong value"
#endif
#if UINT_FAST8_MAX != __UINT_FAST8_MAX__
#error "UINT_FAST8_MAX not usable in #if or wrong value"
#endif
#if INT_FAST16_MIN != -__INT_FAST16_MAX__-1
#error "INT_FAST16_MIN not usable in #if or wrong value"
#endif
#if INT_FAST16_MAX != __INT_FAST16_MAX__
#error "INT_FAST16_MAX not usable in #if or wrong value"
#endif
#if UINT_FAST16_MAX != __UINT_FAST16_MAX__
#error "UINT_FAST16_MAX not usable in #if or wrong value"
#endif
#if INT_FAST32_MIN != -__INT_FAST32_MAX__-1
#error "INT_FAST32_MIN not usable in #if or wrong value"
#endif
#if INT_FAST32_MAX != __INT_FAST32_MAX__
#error "INT_FAST32_MAX not usable in #if or wrong value"
#endif
#if UINT_FAST32_MAX != __UINT_FAST32_MAX__
#error "UINT_FAST32_MAX not usable in #if or wrong value"
#endif
#if INT_FAST64_MIN != -__INT_FAST64_MAX__-1
#error "INT_FAST64_MIN not usable in #if or wrong value"
#endif
#if INT_FAST64_MAX != __INT_FAST64_MAX__
#error "INT_FAST64_MAX not usable in #if or wrong value"
#endif
#if UINT_FAST64_MAX != __UINT_FAST64_MAX__
#error "UINT_FAST64_MAX not usable in #if or wrong value"
#endif

#if defined(INTPTR_MIN) && INTPTR_MIN != -__INTPTR_MAX__-1
#error "INTPTR_MIN not usable in #if or wrong value"
#endif
#if defined(INTPTR_MAX) && INTPTR_MAX != __INTPTR_MAX__
#error "INTPTR_MAX not usable in #if or wrong value"
#endif
#if defined(UINTPTR_MAX) && UINTPTR_MAX != __UINTPTR_MAX__
#error "UINTPTR_MAX not usable in #if or wrong value"
#endif

#if INTMAX_MIN != -__INTMAX_MAX__-1
#error "INTMAX_MIN not usable in #if or wrong value"
#endif
#if INTMAX_MAX != __INTMAX_MAX__
#error "INTMAX_MAX not usable in #if or wrong value"
#endif
#if UINTMAX_MAX != __UINTMAX_MAX__
#error "UINTMAX_MAX not usable in #if or wrong value"
#endif

#if PTRDIFF_MIN != -__PTRDIFF_MAX__-1
#error "PTRDIFF_MIN not usable in #if or wrong value"
#endif
#if PTRDIFF_MAX != __PTRDIFF_MAX__
#error "PTRDIFF_MAX not usable in #if or wrong value"
#endif

#if SIG_ATOMIC_MIN != __SIG_ATOMIC_MIN__
#error "SIG_ATOMIC_MIN not usable in #if or wrong value"
#endif
#if SIG_ATOMIC_MAX != __SIG_ATOMIC_MAX__
#error "SIG_ATOMIC_MAX not usable in #if or wrong value"
#endif

#if SIZE_MAX != __SIZE_MAX__
#error "SIZE_MAX not usable in #if or wrong value"
#endif

#if WCHAR_MIN != __WCHAR_MIN__
#error "WCHAR_MIN not usable in #if or wrong value"
#endif
#if WCHAR_MAX != __WCHAR_MAX__
#error "WCHAR_MAX not usable in #if or wrong value"
#endif

#if WINT_MIN != __WINT_MIN__
#error "WINT_MIN not usable in #if or wrong value"
#endif
#if WINT_MAX != __WINT_MAX__
#error "WINT_MAX not usable in #if or wrong value"
#endif
