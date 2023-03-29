/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" "-mcpu=*" } } */
/* { dg-options "-march=armv8-m.main+fp -mfloat-abi=softfp" } */

#if defined (__ARM_FEATURE_BTI)
#error "Feature test macro __ARM_FEATURE_BTI should not be defined."
#endif

#if defined (__ARM_FEATURE_PAUTH)
#error "Feature test macro __ARM_FEATURE_PAUTH should not be defined."
#endif
