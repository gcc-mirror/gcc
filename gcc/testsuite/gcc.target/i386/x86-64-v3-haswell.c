/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mabi=sysv -mtune=haswell -march=x86-64-v3" } */

/* Check that -march=x86-64-v3 preserves tuning.  */

/* PCLMUL is not in x86-64-v3, but in -march=haswell.  Make sure that
   it is absent.  */
#ifdef __PCLMUL__
# error __PCLMUL__ is defined
#endif

/* -mtune=haswell tuning is preserved.  */
#ifndef __k8__
# error __k8__ is not defined
#endif
#ifndef __tune_haswell__
# error __tune_haswell__ is not defined
#endif
