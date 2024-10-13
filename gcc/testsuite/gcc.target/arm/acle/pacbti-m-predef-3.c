/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8_1m_main_ok } */
/* { dg-require-effective-target mbranch_protection_ok } */
/* { dg-options "-mbranch-protection=pac-ret+leaf -mfloat-abi=hard --save-temps" } */
/* { dg-add-options arm_arch_v8_1m_main } */

#if defined (__ARM_FEATURE_BTI_DEFAULT)
#error "Feature test macro __ARM_FEATURE_BTI_DEFAULT should be undefined."
#endif

#if !defined (__ARM_FEATURE_PAC_DEFAULT)
#error "Feature test macro __ARM_FEATURE_PAC_DEFAULT should be defined."
#endif

/* { dg-final { scan-assembler-not "\.arch_extension pacbti" } } */
/* { dg-final { scan-assembler "\.eabi_attribute 50, 1" } } */
/* { dg-final { scan-assembler "\.eabi_attribute 52, 1" } } */
/* { dg-final { scan-assembler-not "\.eabi_attribute 74" } } */
/* { dg-final { scan-assembler "\.eabi_attribute 76, 1" } } */
