/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-march=*" } { "-march=armv7-m" } } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=soft" } } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" } { "" } } */
/* { dg-options "-march=armv7-m -mcpu=cortex-m3 -mfloat-abi=soft -mthumb" } */

#ifdef __ARM_FP
#error __ARM_FP should not be defined
#endif

#ifdef __ARM_NEON_FP
#error __ARM_NEON_FP should not be defined
#endif
