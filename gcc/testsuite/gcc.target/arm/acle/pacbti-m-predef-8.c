/* { dg-do compile } */
/* { dg-require-effective-target mbranch_protection_ok } */
/* { dg-additional-options "-march=armv8.1-m.main+fp -mbranch-protection=bti+pac-ret+leaf -mfloat-abi=hard" } */

#if (__ARM_FEATURE_BTI_DEFAULT != 1)
#error "Feature test macro __ARM_FEATURE_BTI_DEFAULT should be defined to 1."
#endif

#if (__ARM_FEATURE_PAC_DEFAULT != 5)
#error "Feature test macro __ARM_FEATURE_PAC_DEFAULT should be defined to 5."
#endif

