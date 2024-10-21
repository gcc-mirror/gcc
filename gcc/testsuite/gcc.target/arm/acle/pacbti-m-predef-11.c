/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8_1m_main_pacbti_ok } */
/* { dg-add-options arm_arch_v8_1m_main_pacbti } */

#if (__ARM_FEATURE_BTI != 1)
#error "Feature test macro __ARM_FEATURE_BTI_DEFAULT should be defined to 1."
#endif

#if (__ARM_FEATURE_PAUTH != 1)
#error "Feature test macro __ARM_FEATURE__PAUTH should be defined to 1."
#endif
