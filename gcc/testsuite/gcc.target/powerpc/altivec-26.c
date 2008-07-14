/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

/* A compiler implementing context-sensitive keywords must define this
   preprocessor macro so that altivec.h does not provide the vector,
   pixel, etc. macros.  */

#ifndef __APPLE_ALTIVEC__
#error __APPLE_ALTIVEC__ not pre-defined
#endif
