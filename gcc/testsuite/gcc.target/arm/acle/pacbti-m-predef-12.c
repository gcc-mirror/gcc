/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8_1m_main_ok } */
/* { dg-add-options arm_arch_v8_1m_main } */
/* { dg-additional-options "-mfloat-abi=softfp" } */

#if defined (__ARM_FEATURE_BTI)
#error "Feature test macro __ARM_FEATURE_BTI should not be defined."
#endif

#if defined (__ARM_FEATURE_PAUTH)
#error "Feature test macro __ARM_FEATURE_PAUTH should not be defined."
#endif
