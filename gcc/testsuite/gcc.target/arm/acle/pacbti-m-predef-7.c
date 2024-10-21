/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8_1m_main_pacbti_ok } */
/* { dg-add-options arm_arch_v8_1m_main_pacbti } */
/* { dg-additional-options "--save-temps -mfloat-abi=hard" } */

#if defined (__ARM_FEATURE_BTI_DEFAULT)
#error "Feature test macro __ARM_FEATURE_BTI_DEFAULT should be undefined."
#endif

#if defined (__ARM_FEATURE_PAC_DEFAULT)
#error "Feature test macro __ARM_FEATURE_PAC_DEFAULT should be undefined."
#endif

/* { dg-final { scan-assembler "\.arch_extension pacbti" } } */
/* { dg-final { scan-assembler "\.eabi_attribute 50, 2" } } */
/* { dg-final { scan-assembler "\.eabi_attribute 52, 2" } } */
/* { dg-final { scan-assembler-not "\.eabi_attribute 74" } } */
/* { dg-final { scan-assembler-not "\.eabi_attribute 76" } } */
