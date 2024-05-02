/* PR target/101865 */
/* { dg-do preprocess } */
/* { dg-options "-mdejagnu-cpu=power9 -mno-vsx" } */

/* Verify _ARCH_PWR8 is defined for -mcpu=power9 and after disabling vsx.
   This also confirms __ALTIVEC__ remains set when VSX is disabled.  */

#ifndef _ARCH_PWR7
#error "_ARCH_PWR7 should be defined for this test"
#endif

#ifndef _ARCH_PWR8
#error "_ARCH_PWR8 should be defined for this test"
#endif

#ifndef _ARCH_PWR9
#error "_ARCH_PWR9 should be defined for this test"
#endif

#ifndef __ALTIVEC__
#error "__ALTIVEC__ should be defined for this test"
#endif

#ifdef __VSX__
#error "__VSX__ should not be defined for this test"
#endif
