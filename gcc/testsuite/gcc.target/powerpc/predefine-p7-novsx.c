/* PR target/101865 */
/* { dg-do preprocess } */
/* { dg-options "-mdejagnu-cpu=power7 -mno-vsx" } */

/* Verify we correctly set the correct set of predefined macros
   for the given set of options.  */

#ifndef _ARCH_PWR7
#error "_ARCH_PWR7 should be defined for this test"
#endif

#ifndef __ALTIVEC__
#error "__ALTIVEC__ should be defined for this test"
#endif

#ifdef _ARCH_PWR8
#error "_ARCH_PWR8 should not be defined for this test"
#endif

#ifdef __VSX__
#error "__VSX__ should not be defined for this test"
#endif
