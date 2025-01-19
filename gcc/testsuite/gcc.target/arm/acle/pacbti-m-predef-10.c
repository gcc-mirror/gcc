/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8_1m_main_ok } */
/* { dg-require-effective-target mbranch_protection_ok } */
/* { dg-additional-options "-mbranch-protection=bti+pac-ret -mfloat-abi=hard" } */
/* { dg-add-options arm_arch_v8_1m_main } */

#if (__ARM_FEATURE_BTI_DEFAULT != 1)
#error "Feature test macro __ARM_FEATURE_BTI_DEFAULT should be defined to 1."
#endif

#if !defined (__ARM_FEATURE_PAC_DEFAULT)
#error "Feature test macro __ARM_FEATURE_PAC_DEFAULT should be defined."
#endif
