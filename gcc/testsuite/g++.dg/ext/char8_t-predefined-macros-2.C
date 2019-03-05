// Test that char8_t related predefined macros are present when -fchar8_t is
// enabled.
// { dg-do compile }
// { dg-options "-fchar8_t" }

#if !defined(__CHAR8_TYPE__)
#error __CHAR8_TYPE__ is not defined!
#endif

#if !defined(__GCC_ATOMIC_CHAR8_T_LOCK_FREE)
#error __GCC_ATOMIC_CHAR8_T_LOCK_FREE is not defined!
#endif
