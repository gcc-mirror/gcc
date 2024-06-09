/* PR target/101865 */
/* { dg-do preprocess } */
/* { dg-options "-mdejagnu-cpu=power8 -mno-altivec -mno-vsx" } */

/* Verify _ARCH_PWR8 is defined for -mcpu=power8 and after disabling
   both altivec and vsx.  */

#ifndef _ARCH_PWR7
#error "_ARCH_PWR7 should be defined for this test"
#endif

#ifndef _ARCH_PWR8
#error "_ARCH_PWR8 should be defined for this test"
#endif

#ifdef _ARCH_PWR9
#error "_ARCH_PWR9 should not be defined for this test"
#endif

#ifdef __ALTIVEC__
#error "__ALTIVEC__ should not be defined for this test"
#endif

#ifdef __VSX__
#error "__VSX__ should not be defined for this test"
#endif
