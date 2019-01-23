// Test that char8_t related predefined macros are not present when -fchar8_t is
// not enabled.
// { dg-do compile }
// { dg-options "-fno-char8_t" }

#if defined(__CHAR8_TYPE__)
#error __CHAR8_TYPE__ is defined!
#endif

#if defined(__GCC_ATOMIC_CHAR8_T_LOCK_FREE)
#error __GCC_ATOMIC_CHAR8_T_LOCK_FREE is defined!
#endif
