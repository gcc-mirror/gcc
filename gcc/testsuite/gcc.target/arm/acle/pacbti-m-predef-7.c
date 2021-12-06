/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" "-mcpu=*" } } */
/* { dg-additional-options "-march=armv8.1-m.main+pacbti+fp --save-temps -mfloat-abi=hard" } */

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
