/* For each of several ARM architecture features, check that relevant
   macros are defined or not, and that they have the expected values.  */

#ifdef NEED_ARM_ARCH
# ifdef __ARM_ARCH
#  if __ARM_ARCH != VALUE_ARM_ARCH
#   error __ARM_ARCH has unexpected value
#  endif
# else
#  error __ARM_ARCH is not defined but should be
# endif
#else
# ifdef __ARM_ARCH
#  error __ARM_ARCH is defined but should not be
# endif
#endif

#ifdef NEED_ARM_ARCH_ISA_ARM
# ifdef __ARM_ARCH_ISA_ARM
#  if __ARM_ARCH_ISA_ARM != VALUE_ARM_ARCH_ISA_ARM
#   error __ARM_ARCH_ISA_ARM has unexpected value
#  endif
# else
#  error __ARM_ARCH_ISA_ARM is not defined but should be
# endif
#else
# ifdef __ARM_ARCH_ISA_ARM
#  error __ARM_ARCH_ISA_ARM is defined but should not be
# endif
#endif

#ifdef NEED_ARM_ARCH_ISA_THUMB
# ifdef __ARM_ARCH_ISA_THUMB
#  if __ARM_ARCH_ISA_THUMB != VALUE_ARM_ARCH_ISA_THUMB
#   error __ARM_ARCH_ISA_THUMB has unexpected value
#  endif
# else
#  error __ARM_ARCH_ISA_THUMB is not defined but should be
# endif
#else
# ifdef __ARM_ARCH_ISA_THUMB
#  error __ARM_ARCH_ISA_THUMB is defined but should not be
# endif
#endif

#ifdef NEED_ARM_ARCH_PROFILE
# ifdef __ARM_ARCH_PROFILE
#  if __ARM_ARCH_PROFILE != VALUE_ARM_ARCH_PROFILE
#   error __ARM_ARCH_PROFILE has unexpected value
#  endif
# else
#  error __ARM_ARCH_PROFILE is not defined but should be
# endif
#else
# ifdef __ARM_ARCH_PROFILE
#  error __ARM_ARCH_PROFILE is defined but should not be
# endif
#endif

#ifdef NEED_ARM_FEATURE_UNALIGNED
# ifdef __ARM_FEATURE_UNALIGNED
#  if __ARM_FEATURE_UNALIGNED != VALUE_ARM_FEATURE_UNALIGNED
#   error __ARM_FEATURE_UNALIGNED has unexpected value
#  endif
# else
#  error __ARM_FEATURE_UNALIGNED is not defined but should be
# endif
#else
# ifdef __ARM_FEATURE_UNALIGNED
#  error __ARM_FEATURE_UNALIGNED is defined but should not be
# endif
#endif

#ifdef NEED_ARM_FEATURE_LDREX
# ifdef __ARM_FEATURE_LDREX
#  if __ARM_FEATURE_LDREX != VALUE_ARM_FEATURE_LDREX
#   error __ARM_FEATURE_LDREX has unexpected value
#  endif
# else
#  error __ARM_FEATURE_LDREX is not defined but should be
# endif
#else
# ifdef __ARM_FEATURE_LDREX
#  error __ARM_FEATURE_LDREX is defined but should not be
# endif
#endif

#ifdef NEED_ARM_FEATURE_CLZ
# ifdef __ARM_FEATURE_CLZ
#  if __ARM_FEATURE_CLZ != VALUE_ARM_FEATURE_CLZ
#   error __ARM_FEATURE_CLZ has unexpected value
#  endif
# else
#  error __ARM_FEATURE_CLZ is not defined but should be
# endif
#else
# ifdef __ARM_FEATURE_CLZ
#  error __ARM_FEATURE_CLZ is defined but should not be
# endif
#endif

#ifdef NEED_ARM_FEATURE_DSP
# ifdef __ARM_FEATURE_DSP
#  if __ARM_FEATURE_DSP != VALUE_ARM_FEATURE_DSP
#   error __ARM_FEATURE_DSP has unexpected value
#  endif
# else
#  error __ARM_FEATURE_DSP is not defined but should be
# endif
#else
# ifdef __ARM_FEATURE_DSP
#  error __ARM_FEATURE_DSP is defined but should not be
# endif
#endif

#ifdef NEED_ARM_FEATURE_SIMD32
# ifdef __ARM_FEATURE_SIMD32
#  if __ARM_FEATURE_SIMD32 != VALUE_ARM_FEATURE_SIMD32
#   error __ARM_FEATURE_SIMD32 has unexpected value
#  endif
# else
#  error __ARM_FEATURE_SIMD32 is not defined but should be
# endif
#else
# ifdef __ARM_FEATURE_SIMD32
#  error __ARM_FEATURE_SIMD32 is defined but should not be
# endif
#endif

#ifdef NEED_ARM_FEATURE_QBIT
# ifdef __ARM_FEATURE_QBIT
#  if __ARM_FEATURE_QBIT != VALUE_ARM_FEATURE_QBIT
#   error __ARM_FEATURE_QBIT has unexpected value
#  endif
# else
#  error __ARM_FEATURE_QBIT is not defined but should be
# endif
#else
# ifdef __ARM_FEATURE_QBIT
#  error __ARM_FEATURE_QBIT is defined but should not be
# endif
#endif

#ifdef NEED_ARM_FEATURE_SAT
# ifdef __ARM_FEATURE_SAT
#  if __ARM_FEATURE_SAT != VALUE_ARM_FEATURE_SAT
#   error __ARM_FEATURE_SAT has unexpected value
#  endif
# else
#  error __ARM_FEATURE_SAT is not defined but should be
# endif
#else
# ifdef __ARM_FEATURE_SAT
#  error __ARM_FEATURE_SAT is defined but should not be
# endif
#endif
