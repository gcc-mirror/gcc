/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mabi=sysv -march=skylake -march=x86-64-v3" } */

/* Check that -march=x86-64-v3 overrides a previous -march= setting.  */

/* PCLMUL is not in x86-64-v3, but in -march=skylake.  Make sure that
   it is absent.  */
#ifdef __PCLMUL__
# error __PCLMUL__ is defined
#endif

/* -march=skylake tuning is deactivated.  */
#ifndef __k8__
# error __k8__ is not defined
#endif
#ifdef __skylake__
# error __skylake__ is defined
#endif
#ifdef __tune_skylake__
# error __tune_skylake__ is defined
#endif
