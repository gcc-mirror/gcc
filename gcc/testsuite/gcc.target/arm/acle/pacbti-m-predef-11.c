/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" "-mcpu=*" "-mfloat-abi=*" } } */
/* { dg-options "-march=armv8.1-m.main+pacbti" } */

#if (__ARM_FEATURE_BTI != 1)
#error "Feature test macro __ARM_FEATURE_BTI_DEFAULT should be defined to 1."
#endif

#if (__ARM_FEATURE_PAUTH != 1)
#error "Feature test macro __ARM_FEATURE__PAUTH should be defined to 1."
#endif
