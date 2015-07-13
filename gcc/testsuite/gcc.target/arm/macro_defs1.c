/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-march=*" } { "-march=armv6-m" } } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" } { "" } } */
/* { dg-options "-march=armv6-m -mthumb" } */

#ifdef __ARM_NEON_FP
#error __ARM_NEON_FP should not be defined
#endif

