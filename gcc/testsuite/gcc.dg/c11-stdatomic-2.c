/* Test stdatomic.h header contents.  Test that ATOMIC_*_LOCK_FREE
   macros can be used in an #if directive (DR#458).  */
/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stdatomic.h>

#if ATOMIC_BOOL_LOCK_FREE
#endif
#if ATOMIC_CHAR_LOCK_FREE
#endif
#if ATOMIC_CHAR16_T_LOCK_FREE
#endif
#if ATOMIC_CHAR32_T_LOCK_FREE
#endif
#if ATOMIC_WCHAR_T_LOCK_FREE
#endif
#if ATOMIC_SHORT_LOCK_FREE
#endif
#if ATOMIC_INT_LOCK_FREE
#endif
#if ATOMIC_LONG_LOCK_FREE
#endif
#if ATOMIC_LLONG_LOCK_FREE
#endif
#if ATOMIC_POINTER_LOCK_FREE
#endif
