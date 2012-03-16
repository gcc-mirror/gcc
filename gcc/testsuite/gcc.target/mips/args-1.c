/* Check that certain preprocessor macros are defined, and do some
   consistency checks.  */
/* { dg-do compile } */

const char *compiled_for = _MIPS_ARCH;
const char *optimized_for = _MIPS_TUNE;

#if __mips_fpr != 32 && __mips_fpr != 64
#error Bad __mips_fpr
#endif

/* Test complementary macro pairs: exactly one of each pair
   must be defined.  */

#if defined (_R3000) == defined (_R4000)
#error _R3000 / _R4000 mismatch
#endif

#if defined (__mips_hard_float) == defined (__mips_soft_float)
#error __mips_hard_float / __mips_soft_float mismatch
#endif

#if defined (_MIPSEL) == defined (_MIPSEB)
#error _MIPSEL / _MIPSEB mismatch
#endif

/* Check for __mips64 consistency.  */

#if defined (__mips64) != defined (_R4000)
#error __mips64 / _R4000 mismatch
#endif

#if defined (__mips64) && __mips != 3 && __mips != 4 && __mips != 64
#error __mips64 / __mips mismatch
#endif
